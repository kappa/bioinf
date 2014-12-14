(defn random-by-distr [P]
  (let [sum-p (apply + P)
        norm (vec (map #(/ % sum-p) P))
        sums (vec (reduce #(conj %1 (+ %2 (peek %1)))
                     (vector (first norm)) (rest norm)))
        rand-n (rand)]

    (first (filter #(> (sums %) rand-n) (range 0 (count sums))))
    ))

(prn (random-by-distr [0.1 0.2 0.4 0.3]))
