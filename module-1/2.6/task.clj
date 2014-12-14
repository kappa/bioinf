#! /usr/bin/env clj

(defn do-count-subs [pattern text acc]
  (cond
    (empty? text) acc
    (< (count text) (count pattern)) acc
    :else
      (recur pattern (subs text 1) (+ acc (if (= pattern (subs text 0 (count pattern))) 1 0)))))

(let [text    (read-line)
      pattern (read-line)]
  (println (do-count-subs pattern text 0)))
