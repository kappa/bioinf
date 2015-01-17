;; { "1" [["3" 33] ["4" 2]]
;;   "3" [] }
;; ["1" "3" "4"]

(use '[clojure.string :only (split)])

(def source (read-line))
(def sink (read-line))

(defn add-edge [graph troyka]
  (update-in graph [(first troyka)] #(conj (or % []) (vector (second troyka) (Integer/parseInt (last troyka))))))

(defn add-edge-rev [graph troyka]
  (update-in graph [(second troyka)] #(conj (or % []) (vector (first troyka) (last troyka)))))

(defn read-adjlist []
  (reduce add-edge
          {}
          (map #(vec (split % #"[->:]+")) (line-seq (java.io.BufferedReader. *in*)))))

(def graph (read-adjlist))

(defn find-inboundless [graph]
  (let [have-inbound (set (mapcat #(map first %) (vals graph)))]
    (first (filter (complement have-inbound) (keys graph)))))

(defn find-inboundless-rev [graph removed]
  (let [all-vertices (clojure.set/difference (set (mapcat #(map first %) (vals graph))) removed)]
    (first (filter (complement graph) (seq all-vertices)))))

(prn graph)
;(prn (find-inboundless graph))

(defn toposort [graph topo-seq]
  (cond
    (empty? graph) topo-seq
    :else (let [next-vertex (find-inboundless graph)]
            (recur (dissoc graph next-vertex) (conj topo-seq next-vertex)))))

(def topo-seq (toposort graph []))
(prn topo-seq)

(defn compute-rev-list [graph]
  (reduce add-edge-rev
          {}
          (for [from (keys graph)
                edge (graph from)]
            [from (first edge) (last edge)])))

(def rev-graph (compute-rev-list graph))
(prn "REV" (into (sorted-map) rev-graph))

(defn compute-score [scores vertex]
  (let [do-compute #(+ (first (or (scores (first %)) [-99999 "XXX"])) (second %))
        maxlayan (apply max-key do-compute (rev-graph vertex) )
        new-score (do-compute maxlayan)]
    (assoc scores vertex [new-score (first maxlayan)])))

(def topo-source->sink
  (filter rev-graph (concat (take-while #(not= % sink) (rest (drop-while #(not= % source) topo-seq))) [sink])))

(prn "TOPOSORT seq" topo-source->sink)

(def scores (reduce compute-score
                    {source [0 source]}
                    topo-source->sink))
(prn scores)

(defn backtrack [scores path-vec]
  (cond
    (= (peek path-vec) source) path-vec
    :else (recur scores (conj path-vec (last (scores (last path-vec)))))))

(println (first (scores sink)))
(println (clojure.string/join "->" (rseq (backtrack scores [sink]))))
