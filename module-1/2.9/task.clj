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

(defn all-kmers [genom k kmers]
  (cond
    (< (count genom) k) kmers
    true (recur (rest genom) k (conj kmers (take k genom)))
  ))

(let [genom (seq (read-line))
      k     (Integer/parseInt (read-line))]

      (println (map string (top-counts (reverse (sort-by val (frequencies (all-kmers genom k '[])))))))
  )
