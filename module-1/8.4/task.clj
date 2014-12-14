(defn compute-hamming [p q]
  (cond
    (empty? p) 0
    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
    ))

(defn find-approx-subs [genome pattern mis pos poss]
  (cond
    (< (count genome) (count pattern)) poss
    :else (recur (rest genome) pattern mis (inc pos)
           (if (<= (compute-hamming (take (count pattern) genome) pattern) mis) (conj poss pos) poss))
  ))

(let [pattern (seq (read-line))
      genome (seq (read-line))
      mismatches (Integer/parseInt (read-line))]
  
  (println (find-approx-subs genome pattern mismatches 0 '[])))
