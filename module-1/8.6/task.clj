(defn compute-hamming [p q]
  (cond
    (empty? p) 0
    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
    ))

(defn count-approx-subs [genome pattern mis num-subs]
  (cond
    (< (count genome) (count pattern)) num-subs
    :else (recur (rest genome) pattern mis
           (if (<= (compute-hamming (take (count pattern) genome) pattern) mis) (inc num-subs) num-subs))
  ))

(let [genome (seq (read-line))
      pattern (seq (read-line))
      mismatches (Integer/parseInt (read-line))]
  
  (println (count-approx-subs genome pattern mismatches 0)))
