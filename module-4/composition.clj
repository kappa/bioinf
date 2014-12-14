(defn generate-kmers [k string]
  (sort (map #(subvec string % (+ % k)) (range 0 (inc (- (count string) k))))))


(defn print-vectors [vectors]
  (println (clojure.string/join "\n" (map clojure.string/join vectors))))

(def K (Integer/parseInt (read-line)))
(def string (read-line))

(print-vectors (generate-kmers K (vec (seq string))))
