;;(def incidency-list {"1" ["2" "3"] "3" ["2"] "2" ["1"]})
(def incidency-list {"0" ["3"] "1" ["0"] "2" ["1" "6"] "3" ["2"] "4" ["2"] "5" ["4"] "6" ["5" "8"] "7" ["9"] "8" ["7"] "9" ["6"]})

(defn walk-leo [graph next-vertex current-cycle]
  (println graph next-vertex current-cycle)
  (cond
    (empty? current-cycle) (recur graph (first (get graph next-vertex)) (vector next-vertex))
    (nil? next-vertex) [graph current-cycle]
    :else (recur
            (update-in graph [(peek current-cycle)] rest)
            (first (get graph next-vertex))
            (conj current-cycle next-vertex))))

;(walk-leo incidency-list "0" [])

(defn rotate-while [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn unstick-path [path graph]
  (if (empty? path) path (let [rotated (rotate-while #(empty? (graph %)) (rest path))]
    (vec rotated))))

(unstick-path [] incidency-list)
(first (keys incidency-list))

(defn find-euler-cycle [incidency current-cycle]
  (loop [[graph path] [incidency current-cycle]]
;;    (println graph path)
;;    (read-line)
    (cond
      (every? empty? (vals graph)) path
      :else (let [new-path (unstick-path path graph)]
;;              (println "unstuck path " new-path)
              (recur (walk-leo
                       graph
                       (or (first new-path) (first (keys graph)))
                       new-path))))))

(or (first []) (first (keys incidency-list)))

;;(find-euler-cycle incidency-list [])

(defn split-to-edge [str]
  (let [spl (clojure.string/split str #" ")]
    [(spl 0) (clojure.string/split (spl 2) #",")]))

;;(println (split-to-edge "555 -> 333,999"))

(def graph (apply hash-map (mapcat split-to-edge (line-seq (java.io.BufferedReader. *in*)))))

;;(println graph)

(println (clojure.string/join "->" (find-euler-cycle graph [])))
