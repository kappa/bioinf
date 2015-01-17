(def s (read-line))
(def t (read-line))

(defn minus-two []
  (map - (filter even? (range))))

(defn fill-zeros-fit [columns lines]
  (vec (cons (vec (repeat (inc columns) [0 3]))
             (take lines (map #(vector [% 0]) (rest (minus-two)))))))

(prn (fill-zeros-fit (count s) (count t)))

(defn get-score [scores line col]
  (let [el (get-in scores [line col])]
    (if (vector? el) (first el) el)))

(defn fill-rest-reducer [scores [line col]]
  (prn "L C:" line col)
;  (prn "BL chars:" (get s (dec col)) (get t (dec line)))
;  (prn "BL:" (get-in blosum [(str (get s (dec col))) (str (get t (dec line)))]))
  (let
    [step [
           (- (get-score scores (dec line) col) 2)
           (- (get-score scores line (dec col)) 2)
           (+ (get-score scores (dec line) (dec col))
                  (if (= (get s (dec col)) (get t (dec line))) 1 -2))
           ]]
    (assoc-in scores [line col]
              [(apply max step) (max-key step 2 1 0)])))

(defn fill-rest [scores]
  (reduce fill-rest-reducer
          scores
          (for [line (range 1 (inc (count t)))
                column (range 1 (inc (count s)))] [line column])))

(defn backtrack [scores line col path]
  (prn "BACK: " line col path)
  (cond
    (and (zero? line) (zero? col)) [path 0 0]
    (= (get-in scores [line col 1]) 3) [path line col]
    (= (get-in scores [line col 1]) 0) (backtrack scores (dec line) col (conj path \|))
    (= (get-in scores [line col 1]) 1) (backtrack scores line (dec col) (conj path \-))
    :else (backtrack scores (dec line) (dec col) (conj path \\))))

(def walked (fill-rest (fill-zeros-fit (count s) (count t))))

;(prn "WALKED:")
;(doseq [line walked]
;  (prn line))

(def end-col (dec (count (walked 0)))) ; last column
(def end-line (apply max-key #(get-score walked % end-col) (range (count walked))))

(prn "END: " end-line end-col)

(def backtracked (backtrack walked end-line end-col []))

(def align-mask (reverse (first backtracked)))

(def start-line (second backtracked))
(def start-col (last backtracked))

(println (get-in walked [end-line end-col 0]))

(defn print-masked [mask string skip result]
  (if (seq mask) 
    (if (= (first mask) skip)
      (recur (rest mask) string skip (conj result \-))
      (recur (rest mask) (rest string) skip (conj result (first string))))
    result))

(println (apply str (print-masked align-mask (drop start-col (str s)) \| [])))
(println (apply str (print-masked align-mask (drop start-line (str t)) \- [])))
