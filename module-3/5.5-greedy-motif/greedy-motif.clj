#! /usr/bin/env clj

(defn compute-hamming [p q]
  (cond
    (empty? p) 0
    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
    ))

(defn find-all-kmers [genome k kmers]
  (cond
    (< (count genome) k) kmers
    true (recur (rest genome) k (conj kmers (vec (take k genome))))
  ))

;(prn (find-all-kmers (vec (str "ACCCTGT")) 3 []))

(defn print-vectors [vec-seq]
  (println (clojure.string/join "\n" (map #(apply str %) vec-seq))))

(defn compute-probability [kmer profile]
  (apply * (map #(get (profile %) (kmer %)) (range 0 (count kmer)))))

(defn find-most-probable [text k profile]
  (apply max-key #(compute-probability % profile) (reverse (find-all-kmers text k []))))

(defn get-probs-map [motifs i]
  (let [freq-map (frequencies (map #(get % i) motifs))]
    (apply hash-map (mapcat #(list % (/ (get freq-map % 0) (count motifs))) (str "ACGT")))
    ))

;(prn (get-probs-map [(vec (str "ACGT")) (vec (str "TTTT"))] 1))

(defn compute-profile [motifs]
  {:pre [(pos? (count motifs))]}
  
  (vec (map #(get-probs-map motifs %)
            (range 0 (count (motifs 0))))))

;(prn (compute-profile [(vec (str "ACGT")) (vec (str "TTTT"))]))

(defn generate-motifs-iter [Dna motifs]
  (cond
    (= (count motifs) (count Dna)) motifs
    :else (recur Dna
                 (conj motifs
                       (find-most-probable (get Dna (count motifs))
                                           (count (motifs 0))
                                           (compute-profile motifs))))))

(defn get-most-freq-char [motifs pos]
  (let [freq (frequencies (map #(get % pos) motifs))]
    (apply max-key freq (keys freq))
    ))

;(prn (get-most-freq-char [(vec (str "AGGT")) (vec (str "ACGT")) (vec (str "TTTA"))] 3))

(defn compute-consensus [motifs]
  (vec (map #(get-most-freq-char motifs %) (range 0 (count (motifs 0))))))

;(prn (compute-consensus [(vec (str "ACGT")) (vec (str "TTTT"))]))

(defn compute-score [motifs]
  (let [consensus (compute-consensus motifs)]
    (apply + (map #(compute-hamming % consensus) motifs))))

;(prn (compute-score [(vec (str "ACGT")) (vec (str "TTTT"))]))

(defn find-motifs-greedy [Dna K] 
  (apply min-key #(compute-score %)
           (map #(generate-motifs-iter Dna (vector %))
                (find-all-kmers (Dna 0) K []))))

(let [[K T] (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
      Dna (vec (map #(vec (seq %)) (line-seq (java.io.BufferedReader. *in*))))]

  (prn (vector (vec (take K (Dna 0)))))
  (prn (generate-motifs-iter Dna (vector (vec (take K (drop 0 (Dna 0)))))))
  (print-vectors (find-motifs-greedy Dna K))
;  (prn (find-motifs-greedy Dna K))
  
  )
