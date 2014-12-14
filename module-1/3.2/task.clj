#!/usr/bin/env clj

(def compl-map
  (let [nuc (seq "ACGT")]
    (zipmap nuc (reverse nuc))))

(println
  (apply str
    (reverse (map #(compl-map %) (seq (read-line))))))
