(use '[leiningen.exec :only (deps)])
(deps '[[org.clojure/math.combinatorics "0.0.8"]])

(ns kappa.bin
    (:require [clojure.math.combinatorics :as combo]))

(defn walk-leo [graph next-vertex current-cycle]
  (println graph next-vertex current-cycle)
  (cond
    (empty? current-cycle) (recur graph (first (get graph next-vertex)) (vector next-vertex))
    (nil? next-vertex) [graph current-cycle]
    :else (let [new-graph (update-in graph [(peek current-cycle)] rest)]
            (recur
              new-graph
              (first (get new-graph next-vertex))
              (conj current-cycle next-vertex)))))

(defn rotate-while [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn unstick-path [path graph]
  (if (empty? path)
    path
    (let [rotated (rotate-while #(empty? (graph %)) (rest path))]
      (vec rotated))))

(defn find-euler-cycle [incidency current-cycle]
  (loop [[graph path] [incidency current-cycle]]
;    (println graph path)
;;    (read-line)
    (cond
      (every? empty? (vals graph)) path
      :else (let [new-path (unstick-path path graph)]
;;              (println "unstuck path " new-path)
              (recur (walk-leo
                       graph
                       (or (first new-path) (first (keys graph)))
                       new-path))))))

(defn butlast-str [str]
  (subs str 0 (dec (count str))))

(defn rest-str [str]
  (subs str 1 (count str)))

;;(butlast-str "ACDDD")
;;(rest-str "XYZ")

(defn add-edge-to-graph [graph-map edge-str]
  (update-in graph-map [(butlast-str edge-str)] #(vec (conj % (rest-str edge-str)))))

(defn generate-de-bruijn-patterns [patterns]
  (reduce add-edge-to-graph (sorted-map) patterns))

(defn generate-all-binaries [k]
  (map #(apply str %) (apply combo/cartesian-product (repeat k ["0" "1"]))))

(def graph
  (let [k (Integer/parseInt (read-line))
        patterns (generate-all-binaries k)]
    (generate-de-bruijn-patterns patterns)))

;(println graph)

(let [path (rest (find-euler-cycle graph []))]
  (println "path: " path)
  (print (first path))
  (println (clojure.string/join (map last (rest path))))
  )
