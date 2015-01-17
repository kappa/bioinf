;Question 3
;Imagine a hypothetical world in which there are two amino acids, X and Z, having respective masses 2 and 3. How many linear peptides can be formed from these amino acids having mass equal to 24?  (Remember that the order of amino acids matters.)

(defn generate-sum [total result]
  (cond
    (zero? total) (prn result)
    (>= total 3) (do
                   (generate-sum (- total 3) (conj result 3))
                   (generate-sum (- total 2) (conj result 2)))
    (== total 2) (generate-sum (- total 2) (conj result 2))
    :else nil))

(generate-sum 24 [])
