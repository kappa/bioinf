(def nm (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))) ; lines
(def n (first nm))
(def m (last nm))

(defn read-integer-line []
  (mapv #(Integer/parseInt %) (clojure.string/split (read-line) #" ")))

(def down  (vec (repeatedly n read-integer-line)))
(read-line)
(def right  (vec (repeatedly (inc n) read-integer-line)))

(prn "Down")
(prn down)
(prn "Right")
(prn right)

(defn fill-first-column [s]
  (reduce #(assoc-in %1 [%2 0] (+ (get-in down [(dec %2) 0]) (get-in %1 [(dec %2) 0])))
          s
          (range 1 (inc n))))

(defn fill-first-line [s]
  (reduce #(assoc-in %1 [0 %2] (+ (get-in right [0 (dec %2)]) (get-in %1 [0 (dec %2)])))
          s
          (range 1 (inc m))))

(defn fill-rest-reducer [s line-col]
  (prn "S " s) (prn "LC: " line-col)

  (let
    [line (first line-col)
     col  (last line-col)]

    (assoc-in s line-col (max (+ (get-in down [(dec line) col]) (get-in s [(dec line) col]))
                              (+ (get-in right [line (dec col)]) (get-in s [line (dec col)])))))
  )

(defn fill-rest [s]
  (reduce fill-rest-reducer
          s
          (for [line (range 1 (inc n))
                column (range 1 (inc m))] [line column])))

(prn (fill-rest (fill-first-line (fill-first-column (vec (cons [0] (repeat n [])))))))

;; n = 2 (3 lines)
;; m = 3 (4 columns)
;;    0     1     2     3
;; 0 [0] 3 [3] 2 [5] 1 [6]
;;    1     2     3     4
;; 1 [1] 1 [5] 2 [8] 3 [11]
;;    4     3     2     1
;; 2 [5] 2 [8] 2 [10] 2[12]
;;
;; [ [0 3 5 6] [1 5 8 11] [5 8 10 12] ]
;; 
;; 1 2 3 4
;; 4 3 2 1
;; -
;; 3 2 1
;; 1 2 3
;; 2 2 2
