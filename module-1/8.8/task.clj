#! /usr/bin/env clj

(use '[leiningen.exec :only (deps)])
(deps '[[com.taoensso/timbre "3.3.1"]])

(require '[taoensso.timbre.profiling :as profiling
                      :refer (pspy pspy* profile defnp p p*)])

(defn string [list]
  (clojure.string/join "" list))

(defn top-counts-eq [counts cnt tops]
  (cond
    (empty? counts) tops
    (< (val (first counts)) cnt) tops
    true (recur (rest counts) cnt (conj tops (key (first counts))))
    ))

(defn top-counts [counts]
  (cond
    (empty? counts) nil
    true (top-counts-eq counts (val (first counts)) '[])))

(defn find-all-kmers [genome k kmers]
  (cond
    (< (count genome) k) kmers
    true (recur (rest genome) k (conj kmers (vec (take k genome))))
  ))

;(defn compute-hamming [p q]
;  (cond
;    (empty? p) 0
;    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
;    ))

(defn compute-hamming [p q sum]
  (cond
    (empty? p) sum
    :else (recur (rest p) (rest q) (+ sum (if (= (first p) (first q)) 0 1)))
    ))

(defn hamming-less-eq? [p q ^long sum ^long thres]
  (cond
    (> sum thres) false
    (empty? p) true
    :else (recur (rest p) (rest q) (+ sum (if (= (first p) (first q)) 0 1)) thres)
    ))

(defn generate-neighbors [kmer-vec d]
  (cond
    (= d 0) [kmer-vec]
    (= (count kmer-vec) 1) [[\A] [\C] [\T] [\G]]
    :else (vec (mapcat (fn [rest-neighbor]
                    (if (< (compute-hamming rest-neighbor (rest kmer-vec) 0) d)
                      (map (fn [nuc] (vec (cons nuc rest-neighbor))) '(\A \C \G \T))
                      (list (vec (cons (first kmer-vec) rest-neighbor)))))
                 
                  (generate-neighbors (rest kmer-vec) d)))
    
    ))

(defn ^long count-approx-subs [genome pattern ^long mis ^long num-subs]
  (let [pat-len (count pattern)]
    (cond
      (p :cond-end-in-cas (< (count genome) pat-len)) num-subs
      :else (recur (p :rest-genome-in-cas (rest genome)) pattern mis
            (if (p :hamming-less (hamming-less-eq? (p :take-in-cas (take pat-len genome)) pattern 0 mis)) (inc num-subs) num-subs))
  )))

(def count-approx-subs-memo (memoize count-approx-subs))

(def compl-map
  (let [nuc (seq "ACGT")]
    (zipmap nuc (reverse nuc))))

(defn compute-compl-pattern [pattern]
  (reverse (map #(compl-map %) pattern))
)

(defnp ^long count-approx-subs-w-compl [genome pattern ^long mis]
  (+ (p :casw-left (count-approx-subs genome pattern mis 0)) (p :casw-right (count-approx-subs genome (compute-compl-pattern pattern) mis 0))))

(def count-approx-subs-w-compl-memo (memoize count-approx-subs-w-compl))

(defn count-all-approx-w-compl [genome patterns d counts]
  (cond
    (empty? patterns) counts
    :else (recur genome (rest patterns) d (assoc counts (first patterns) (count-approx-subs-w-compl-memo genome (first patterns) d)))))

(defn find-all-top-counts [counts]
  (top-counts (reverse (sort-by val counts)))
  )

(defn println-vecs [vecs]
  (println (clojure.string/join "\n" (map (fn [vec] (clojure.string/join "" vec)) vecs))))

(profile :info :Task (let [genome (seq (read-line))
      [k d]     (map read-string (clojure.string/split (read-line) #" "))
      all-kmers (find-all-kmers genome k '[])]

    (println (println-vecs (find-all-top-counts (count-all-approx-w-compl genome (mapcat (fn [kmer] (generate-neighbors kmer d)) all-kmers) d '{}))))
;    (println (reverse (sort-by val (count-all-approx genome (mapcat (fn [kmer] (generate-neighbors kmer d)) all-kmers) d '{}))))
  ))
