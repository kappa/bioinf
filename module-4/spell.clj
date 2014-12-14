(defn spell-genome [kmers]
  (into (first kmers) (map peek (rest kmers))))

(defn read-kmers [string]
  (map #(vec (seq %)) (clojure.string/split string #"\n")))

(println (clojure.string/join (spell-genome (read-kmers (slurp "data.txt")))))
