(def masses (vals  {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \N 114, \D 115, \K 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   }))

(defn count-sums [sum]
  (cond
    (< sum 0) 0
    (= sum 0) 1
    :else (apply + (map #(count-sums-memo (- sum %)) masses))))

(def count-sums-memo (memoize count-sums))

(count-sums-memo 1200)
