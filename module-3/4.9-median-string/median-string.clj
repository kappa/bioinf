#! /usr/bin/env clj

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

;;(prn (generate-neighbors (vec (seq "AGCT")) 1))

(defn find-all-kmers [genome k kmers]
  (cond
    (< (count genome) k) kmers
    true (recur (rest genome) k (conj kmers (vec (take k genome))))
  ))

(defn generate-all-kmers [k]
  (if (= k 1)
    (map #(vec (list %)) (seq "ACGT"))
    (mapcat (fn [k-1-mer] (map #(conj k-1-mer %) (seq "ACGT"))) (generate-all-kmers (dec k)))))

(defn print-vectors [vec-seq]
  (println (clojure.string/join " " (map #(apply str %) vec-seq))))

;;(print-vectors (generate-all-kmers 9))

(defn compute-distance [pattern string]
  (apply min (map #(compute-hamming pattern %) (find-all-kmers string (count pattern) []))))

;;(prn (compute-distance (vec (seq "AAA")) (vec (seq "TGGCACTGAA"))))

(defn compute-distance-to-coll [pattern strings]
  (apply + (map #(compute-distance pattern %) strings)))

(defn find-median-string [candidates DNA]
  (apply min-key #(compute-distance-to-coll % DNA) candidates))

(defn find-median-string-all [candidates DNA]
  (let [all-d (map #(vector % (compute-distance-to-coll % DNA)) candidates)
        min-d (apply min (map #(get % 1) all-d))]
    (filter #(= min-d (get % 1)) all-d)
    ))

(let [K (Integer/parseInt (read-line))
      DNA (map #(vec (seq %)) (line-seq (java.io.BufferedReader. *in*)))]

 (prn (find-median-string-all (generate-all-kmers K) DNA)))
; (prn (find-median-string (generate-all-kmers K) DNA)))
