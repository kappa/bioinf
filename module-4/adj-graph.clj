(defn kmer->str [kmer]
  (clojure.string/join kmer))

(defn printstr-edge [from to]
  (clojure.string/join " -> " (list from to)))

(defn adjacent? [from to]
  (= (subs from 1) (subs to 0 (dec (count to)))))

(defn find-adjacency [kmers]
  (filter identity
          (for [left kmers
                right kmers]
            (when (adjacent? left right) (printstr-edge left right)))))

(println
  (clojure.string/join "\n"
                       (find-adjacency
                         (with-open [rdr
                                     (clojure.java.io/reader "/home/kappa/work/bioinf/chapter_4/dataset_198_9.txt")]
         (sort (doall (line-seq rdr)))))))
