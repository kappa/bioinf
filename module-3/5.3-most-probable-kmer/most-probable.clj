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

(defn print-vectors [vec-seq]
  (println (clojure.string/join " " (map #(apply str %) vec-seq))))

(defn compute-probability [kmer profile]
  (apply * (map #(get (profile %) (kmer %)) (range 0 (count kmer)))))

(defn find-most-probable [text k profile]
  (apply max-key #(compute-probability % profile) (reverse (find-all-kmers text k []))))

(defn read-profile []
  (let [lines (vec (map #(vec (clojure.string/split % #" ")) (line-seq (java.io.BufferedReader. *in*))))
        k (count (lines 0))]

    (vec (map #(zipmap (str "ACGT") (map (fn [l] (Float/parseFloat (get (lines l) %))) (range 0 4))) (range 0 k)))
    ))

;;(prn (read-profile))

(let [Text (vec (str (read-line)))
      K (Integer/parseInt (read-line))
      Profile (read-profile)]
 (prn (find-most-probable Text K Profile)))
