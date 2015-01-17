(def s (read-line))
(def t (read-line))

(defn minus-ones []
  (map - (range)))

(defn fill-zeros-leven [columns lines]
  (vec (cons (vec (map #(vector % 1) (take (inc columns) (minus-ones))))
             (take lines (map #(vector [% 0]) (rest (minus-ones)))))))

(defn get-score [scores line col]
  (let [el (get-in scores [line col])]
    (if (vector? el) (first el) el)))

(defn fill-rest-reducer [scores [line col]]
  (let
    [step [
           (- (get-score scores (dec line) col) 1)
           (- (get-score scores line (dec col)) 1)
           (+ (get-score scores (dec line) (dec col))
                  (if (= (get s (dec col)) (get t (dec line))) 0 -1))
           ]]
    (assoc-in scores [line col]
              [(apply max step) (max-key step 2 1 0)])))

(defn fill-rest [scores]
  (reduce fill-rest-reducer
          scores
          (for [line (range 1 (inc (count t)))
                column (range 1 (inc (count s)))] [line column])))

(def walked (fill-rest (fill-zeros-leven (count s) (count t))))

(println (- (get-in walked [(dec (count walked)) (dec (count (walked 0))) 0])))
