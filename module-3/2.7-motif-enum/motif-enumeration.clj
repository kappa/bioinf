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

(defn instr-with-mismatches? [needle haystack d]
  (cond 
    (< (count haystack) (count needle)) false
    (<= (compute-hamming needle (take (count needle) haystack)) d) true
    :else (recur needle (rest haystack) d)
    ))

;;(prn (instr-with-mismatches? (vec (str "AG")) (vec (str "ACAT")) 1))

(defn in-all-with-mismatches? [needle all d]
  (every? #(instr-with-mismatches? needle % d) all))

(defn find-motifs [candidates dna d]
  (filter #(in-all-with-mismatches? % dna d) candidates))

(defn find-candidates [dna k d]
  (distinct (mapcat #(generate-neighbors % d) (mapcat #(find-all-kmers % k []) dna))))

;;(prn (find-candidates DNA 6))

(defn print-vectors [vec-seq]
  (println (clojure.string/join " " (map #(apply str %) vec-seq))))

;;(prn (in-all-with-mismatches? (vec (str "ATA")) DNA 1))
;;(prn (find-motifs [(vec (str "ATA"))] DNA D))

;;(prn (find-candidates DNA K D))

(let [[K D] (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
      DNA (map #(vec (seq %)) (line-seq (java.io.BufferedReader. *in*)))]
 (print-vectors (find-motifs (find-candidates DNA K D) DNA D)))
