(def s (read-line))
(def t (read-line))

(defn fill-zeros [columns lines]
  (vec (cons (vec (repeat columns [0 -1])) (repeat (dec lines) [[0 -1]]))))

(defn get-score [scores line col]
  (let [el (get-in scores [line col])]
    (if (vector? el) (first el) el)))

(defn fill-rest-reducer [scores [line col]]
  ;(prn "L C:" line col)
  (let
    [step [(get-score scores (dec line) col)
           (get-score scores line (dec col))
           (+ (get-score scores (dec line) (dec col))
                  (if (= (get s col) (get t line)) 1 0))]]
    (assoc-in scores [line col]
              [(apply max step) (max-key step 0 2 1)])))

(defn fill-rest [scores]
  (reduce fill-rest-reducer
          scores
          (for [line (range 1 (count t))
                column (range 1 (count s))] [line column])))

(defn backtrack [scores line col string]
  (prn "BACK: " line col string)
  (cond
    (zero? line) (conj string (get t 0))
    (zero? col)  (conj string (get s 0))
    (= (get-in scores [line col 1]) 0) (backtrack scores (dec line) col string)
    (= (get-in scores [line col 1]) 1) (backtrack scores line (dec col) string)
    :else (backtrack scores (dec line) (dec col) (conj string (get t line)))))

(def walked (fill-rest (fill-zeros (count s) (count t))))

(prn walked)

(println (apply str (reverse (backtrack walked
                                        (dec (count t))
                                        (dec (count s))
                                        []))))
