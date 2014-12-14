(def peptide (seq "TLAM"))

(def amino-mass
  {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \L 113, \N 114, \D 115, \K 128, \Q 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })

(defn compute-amino-masses [peptide]
  (map #(amino-mass %) peptide))

(def masses-masses (vec (concat (compute-amino-masses peptide) (compute-amino-masses peptide))))

(def N (count peptide))

(def product (for [start (range 0 N)
           len (range start (dec (+ start N)))]
       [start len]))


(defn theor-cycl-spectrum [peptide]
  (conj (vec (cons 0 (sort (map #(apply + %)
    (map (fn[pair] (subvec masses-masses (pair 0) (inc (pair 1))))
       product))))) (apply + (compute-amino-masses peptide)))
  )

(= '[0 71 101 113 131 184 202 214 232 285 303 315 345 416]
(theor-cycl-spectrum peptide)
)
