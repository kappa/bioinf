(defn butlast-str [str]
  (subs str 0 (dec (count str))))

(defn rest-str [str]
  (subs str 1 (count str)))

;;(butlast-str "ACDDD")
;;(rest-str "XYZ")

(defn add-edge-to-graph [graph-map edge-str]
  (update-in graph-map [(butlast-str edge-str)] #(conj % (rest-str edge-str))))

(defn generate-de-bruijn-patterns [patterns]
  (reduce add-edge-to-graph (sorted-map) patterns))

(defn print-adj-list [graph-map]
  (clojure.string/join "\n" (map #(str (get % 0) " -> " (clojure.string/join "," (get % 1))) (seq graph-map))))

(let [patterns (line-seq (java.io.BufferedReader. *in*))]
  (println (print-adj-list (generate-de-bruijn-patterns patterns))))
