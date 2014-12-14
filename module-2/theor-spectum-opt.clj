(def peptide (seq "NQEL"))

(def amino-mass
  {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \L 113, \N 114, \D 115, \K 128, \Q 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })

(defn compute-amino-masses [peptide]
  (map #(amino-mass %) peptide))

(defn compute-prefix-masses [peptide prefixes]
  (cond
    (empty? peptide) prefixes
    :else (recur (rest peptide) (conj prefixes (+ (last prefixes) (get amino-mass (first peptide)))))
   ))

(compute-prefix-masses peptide [0])

(def N (count peptide))

(for [start (range 0 N)
      end (range start N)]

  [start end])