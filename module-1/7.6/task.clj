(defn compute-all-skews [genome start-skew all-skews]
  (cond
    (empty? genome) all-skews
    :else (recur (rest genome)
           (cond
             (= (first genome) \G) (+ start-skew 1)
             (= (first genome) \C) (- start-skew 1)
             :else start-skew)
           (conj all-skews start-skew))))

(defn find-indexes-min [skews]
  (let [min-skew (apply min skews)]
    (filter (fn [x] (= (get skews x) min-skew)) (range (count skews)))))

(let [genome (seq (read-line))]
  (println (find-indexes-min (compute-all-skews genome 0 '[])))
  )
