#! /usr/bin/env clj

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

(defn generate-mismatched [kmer d next-mis-pos mis-vec]
  (cond
    (= d 0) mis-vec
    (< (- (count kmer) next-mis-pos) d) mis-vec
    :else (recur kmer (dec d) (inc next-mis-pos) (conj mis-vec (assoc kmer next-mis-pos \X)))
    ))

(defn generate-1-mutation [kmer-vec pos]
  (map (fn [nuc] (assoc kmer-vec pos nuc)) '(A T C G)))

(defn generate-all-1nuc-mutations [kmer-vec]
  (mapcat (fn [pos] (generate-1-mutation kmer-vec pos)) (range (count kmer-vec)))
  )

(defn generate-2-mutations [kmer-vec]
  (mapcat generate-all-1nuc-mutations (generate-all-1nuc-mutations kmer-vec)))

(defn generate-all-N-mutations [kmer-vec N]
  (cond
    (= N 0) (list kmer-vec)
    :else (mapcat generate-all-1nuc-mutations (generate-all-N-mutations kmer-vec (dec N)))
    ))

(defn compute-hamming [p q]
  (cond
    (empty? p) 0
    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
    ))


(defn generate-neighbors [kmer-vec d]
  (cond
    (= d 0) [kmer-vec]
    (= (count kmer-vec) 1) [[\A] [\C] [\T] [\G]]
    :else (vec (mapcat (fn [rest-neighbor]
                    (if (< (compute-hamming rest-neighbor (rest kmer-vec)) d)
                      (map (fn [nuc] (vec (cons nuc rest-neighbor))) '(\A \C \G \T))
                      (list (vec (cons (first kmer-vec) rest-neighbor)))))
                 
                  (generate-neighbors (rest kmer-vec) d)))
    
    ))

(defn count-approx-subs [genome pattern mis num-subs]
  (cond
    (< (count genome) (count pattern)) num-subs
    :else (recur (rest genome) pattern mis
           (if (<= (compute-hamming (take (count pattern) genome) pattern) mis) (inc num-subs) num-subs))
  ))

(defn count-all-approx [genome patterns d counts]
  (cond
    (empty? patterns) counts
    :else (recur genome (rest patterns) d (assoc counts (first patterns) (count-approx-subs genome (first patterns) d 0)))))

(defn find-all-top-counts [counts]
  (top-counts (reverse (sort-by val counts)))
  )

(let [genome (seq (read-line))
      d (Integer/parseInt (read-line))]

  (println (clojure.string/join "\n" (map (fn [vec] (clojure.string/join "" vec)) (generate-neighbors (vec genome) d))))
;  (println (clojure.string/join "\n" (map (fn [vec] (clojure.string/join "" vec)) (generate-all-N-mutations (vec genome) d))))

  )
