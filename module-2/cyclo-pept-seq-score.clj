(def spectrum '(0 71 178 202 202 202 333 333 333 404 507 507))
(def peptide (seq "MAMA"))

(def spec-map (frequencies spectrum))
(def total-mass (last spectrum))

(def amino-acids-dict {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \L 113, \N 114, \D 115, \K 128, \Q 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })

(defn compute-amino-masses [peptide]
  (map #(amino-acids-dict %) peptide))

(compute-amino-masses peptide)

(def masses-masses (vec (concat (compute-amino-masses peptide) (compute-amino-masses peptide))))

(str masses-masses)

(def N (count peptide))

(def product (for [start (range 0 N)
           len (range start (dec (+ start N)))]
       [start len]))

(defn theor-cycl-spectrum [peptide]
  (conj (vec (cons 0 (sort (map #(apply + %)
    (map (fn[pair] (subvec masses-masses (pair 0) (inc (pair 1))))
       product))))) (apply + (compute-amino-masses peptide)))
  )

(theor-cycl-spectrum peptide)

(defn score-pepspec-exp [theory spec-map score]
  (let [one (first theory)]
    (cond
      (empty? theory) score
      (not (get spec-map one)) (recur (rest theory) spec-map score)
      (<= (get spec-map one) 0) (recur (rest theory) spec-map score)
      :else (recur (rest theory) (update-in spec-map [one] dec) (inc score)))))

(score-pepspec-exp (theor-cycl-spectrum peptide) spec-map 0)

