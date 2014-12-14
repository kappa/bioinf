(def spectrum '[0 57 118 179 236 240 301])

(defn convolute [spectrum]
  (for [start (range 0 (dec (count spectrum)))
        end (range (inc start) (count spectrum))]

    (- (get spectrum end) (get spectrum start))
  ))

(frequencies (sort (convolute (vec (sort spectrum)))))