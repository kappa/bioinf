(defn compute-hamming [p q]
  (cond
    (empty? p) 0
    :else (+ (if (= (first p) (first q)) 0 1) (compute-hamming (rest p) (rest q)))
    ))

(let [p (read-line)
      q (read-line)]
  
  (println (compute-hamming p q)))
