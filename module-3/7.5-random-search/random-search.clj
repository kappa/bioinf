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
    (apply hash-map (mapcat #(list % (/ (inc (get freq-map % 0)) (+ 4 (count motifs)))) (str "ACGT")))
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

(defn compute-motifs [profile Dna K]
  (vec (map #(find-most-probable % K profile) Dna)))

(defn choose-random-motifs [Dna K]
  (vec (map #(rand-nth (find-all-kmers % K [])) Dna))
  )

(defn iterate-motifs-profile [Dna K motifs best-motifs b-m-score]
  (let [score (compute-score motifs)]
    (cond
      (and (seq best-motifs) (>= score b-m-score))
        (vector best-motifs b-m-score)
      :else (recur Dna
                   K
                   (compute-motifs (compute-profile motifs) Dna K)
                   motifs
                   score))))

(defn run-times [Dna K times]
  (apply min-key #(get % 1)
                 (map #(iterate-motifs-profile
                         Dna K (choose-random-motifs Dna K) [] %)
                      (range 0 times))))

(let [[K T] (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
      Dna (vec (map #(vec (seq %)) (line-seq (java.io.BufferedReader. *in*))))]

  (print-vectors (get (run-times Dna K 100) 0))
  )
