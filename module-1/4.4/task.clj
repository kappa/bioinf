#! /usr/bin/env clj

(use '(clojure set))

(defn find-kmers [genom k kmers]
  (cond
    (< (count genom) k) kmers
    true (recur (rest genom) k (conj kmers (take k genom)))
  ))

(defn find-clumps [genom k t]
  (set (keys (filter (fn [pair] (>= (val pair) t)) (frequencies (find-kmers genom k '[]))))))

(defn find-L-clumps [genom k t L clumps]
  (cond
    (< (count genom) L) clumps
    :else (recur (rest genom) k t L (union clumps (find-clumps genom k t)))
    ))

(defn string [x]
  (clojure.string/join "" x))

(let [genom (seq (read-line))
      [k L t] (map read-string (clojure.string/split (read-line) #" "))]

  (println (clojure.string/join " " (map string (find-L-clumps genom k t L #{}))))
  )
