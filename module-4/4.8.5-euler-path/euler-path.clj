(defn walk-leo [graph next-vertex current-cycle]
;;  (println graph next-vertex current-cycle)
  (cond
    (empty? current-cycle) (recur graph (first (get graph next-vertex)) (vector next-vertex))
    (nil? next-vertex) [graph current-cycle]
    :else (recur
            (update-in graph [(peek current-cycle)] rest)
            (first (get graph next-vertex))
            (conj current-cycle next-vertex))))

(defn rotate-while [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn unstick-path [path graph]
  (if (empty? path) path (let [rotated (rotate-while #(empty? (graph %)) (rest path))]
    (vec rotated))))

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

(defn split-to-edge [str]
  (let [spl (clojure.string/split str #" ")]
    [(spl 0) (clojure.string/split (spl 2) #",")]))

;;(println (split-to-edge "555 -> 333,999"))

(def graph (apply hash-map (mapcat split-to-edge (line-seq (java.io.BufferedReader. *in*)))))

(defn compute-in-degree [degree [from to-vec]]
  (reduce #(update-in %1 [%2] (fnil inc 0)) degree to-vec)
  )

(defn fix-graph [graph]
  (let [in-degree (reduce compute-in-degree {} graph)
        start (first (filter #(< (or (in-degree %) 0) (count (graph %))) (keys graph)))
        end   (first (filter #(> (in-degree %) (count (graph %))) (keys in-degree)))]
;    (println "START END ========== " start end)
;    (println "==== starts " (filter #(< (or (in-degree %) 0) (count (graph %))) (keys graph)))
;    (println "==== ends " (filter #(> (in-degree %) (count (graph %))) (keys in-degree)))
    (vector start end (update-in graph [end] #(vec (conj % start))))))

;(println (fix-graph graph))

(defn break-cycle [euler-cycle start end]
;  (println euler-cycle)
;  (read-line)
  (cond
    (and (= (first euler-cycle) start) (= (last euler-cycle) end)) euler-cycle
    :else (recur
            (rotate-while
              #(not= % start)
              (concat (rest euler-cycle) (vector (first euler-cycle))))
            start
            end)))

(defn find-euler-path [graph]
  (let [[start end fixed-graph] (fix-graph graph)
        euler-cycle (find-euler-cycle fixed-graph [])
       ]
;;    (println euler-cycle)
    (break-cycle (rest euler-cycle) start end)))

(print (clojure.string/join "->" (find-euler-path graph)))
