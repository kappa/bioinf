#! /usr/bin/env clj

(defn starts-with? [s w]
  (cond
    (empty? s) true
    (empty? w) nil
    (= (first s) (first w)) (recur (rest s) (rest w))
    :else nil))

(defn find-subs [s w pos]
  (cond
    (empty? w) nil
    (starts-with? s w) (cons pos (find-subs s (rest w) (inc pos)))
   :else (recur s (rest w) (inc pos))))

(def motif (seq (read-line)))
(def strand (seq (read-line)))

(println (clojure.string/join " " (find-subs motif strand 0)))
