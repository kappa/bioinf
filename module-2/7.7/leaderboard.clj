#! /usr/bin/env clj

(def nucleotides {\G 57 \A 71 \S 87 \P 97 \V 99 \T 101 \C 103 \I 113 \L 113 \N
                  114 \D 115 \K 128 \Q 128 \E 129 \M 131 \H 137 \F 147 \R 156
                  \Y 163 \W 186})

(def nucleotide-masses (sort (distinct (vals nucleotides))))

(defn expand-peptide [peptide]
  (map #(conj peptide %) nucleotide-masses))

;;(prn (expand-peptide []))
;;(prn (expand-peptide [999 666]))

(defn expand-leaderboard [board]
  (mapcat expand-peptide board))

(def board1 (expand-leaderboard '([88] [] [999 666])))
;;(prn board1)

(defn trim-leaderboard [board scorer N]
  (cond
    (empty? board) board
    (<= (count board) N) board
    :else
      (let [sorted-scored-leaderbord (sort-by peek > (pmap #(vector % (scorer %)) board))
            nth-score (- (peek (nth sorted-scored-leaderbord (dec N))) 0)]
    (map first (take-while #(>= (peek %) nth-score) sorted-scored-leaderbord)))))

;;(prn (map #(vector % (apply max %)) board1))

;;(prn (trim-leaderboard board1 #(apply + %) 15))

(defn compute-linear-spectrum [peptide]
  (cons 0 (sort (for [start (range 0 (count peptide))
        end   (range start (count peptide))]
    (apply + (subvec peptide start (inc end)))))))

;;(prn (compute-linear-spectrum [1]))
;;(prn (compute-linear-spectrum [1 2]))
;;(prn (compute-linear-spectrum [1 2 3]))
;;(prn (compute-linear-spectrum [1 2 3 999]))

(defn do-score-against-experiment [peptide-spec spec-freq score]
  (cond
    (empty? peptide-spec) score
    (pos? (or (spec-freq (first peptide-spec)) -1))
      (recur (rest peptide-spec) (update-in spec-freq [(first peptide-spec)] dec) (inc score))
    :else (recur (rest peptide-spec) spec-freq score)))

(defn score-against-experiment [peptide spectrum]
  (do-score-against-experiment (compute-linear-spectrum peptide) (frequencies spectrum) 0))

(defn filter-heavy-from-leaderboard [board spec-mass]
  (filter #(<= (apply + %) spec-mass) board))

(def NN (Integer/parseInt (read-line)))
(def experiment (vec (map #(Integer/parseInt %)
                          (clojure.string/split (read-line) #" "))))
(def exp-spec-mass (last experiment))

;;(prn (trim-leaderboard board1 #(score-against-experiment % experiment) 15))

(defn find-leader [board spec-mass]
  board)
;;  (first (filter #(= spec-mass (apply + %)) board)))
;;  (first board))

(defn sequence-leaderboard [board leader]
  (do ;(prn board leader) (read-line)
  (cond
    (empty? board) leader
    :else (recur (trim-leaderboard
                   (filter-heavy-from-leaderboard (expand-leaderboard board) exp-spec-mass)
                   #(score-against-experiment % experiment)
                   NN)
                 (find-leader board exp-spec-mass)))))

;;(prn (filter-heavy-from-leaderboard (expand-leaderboard '([])) exp-spec-mass))
;;(prn (trim-leaderboard
;;                  (filter-heavy-from-leaderboard (expand-leaderboard '([])) exp-spec-mass)
;;                  #(score-against-experiment % experiment)
;;                  NN))

(defn print-dashed [seq]
  (println (clojure.string/join "-" seq)))

(print-dashed (sequence-leaderboard '([]) [777]))
