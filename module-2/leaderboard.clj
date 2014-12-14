(def spectrum '(0 97 97 97 100 129 194 226 226 226 258 323 323 355 393 452))

(def trim-n 423)

(def spec-map (frequencies spectrum))
(def total-mass (last spectrum))

(def amino-acid {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \N 114, \D 115, \K 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })

(defn new-candidates [peptides]
  (for [peptide peptides
        new-amino (vals amino-acid)]
    (conj peptide new-amino)))

(defn theor-linear-spectrum [peptide]
  (cons 0 (sort (for [start (range 0 (count peptide))
        end (range start (count peptide))]

    (apply + (subvec peptide start (inc end)))))))

(theor-linear-spectrum '[114 128 129 113])

(defn score-pepspec-exp [theory spec-map score]
  (let [one (first theory)]
    (cond
      (empty? theory) score
      (not (get spec-map one)) (recur (rest theory) spec-map score)
      (<= (get spec-map one) 0) (recur (rest theory) spec-map score)
      :else (recur (rest theory) (update-in spec-map [one] dec) (inc score)))))

(def amino-acids-dict {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \L 113, \N 114, \D 115, \K 128, \Q 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })


(map amino-acids-dict (seq "PEEP"))

(score-pepspec-exp (theor-linear-spectrum '[97 129 129 97]) spec-map 0)



(defn find-match [candidates]
  (first (filter #(= (apply + %) total-mass) candidates)))

(defn trim-top [candidates]
  (let [sorted (sort-by last >
                      (map #(vec [% (score-pepspec-exp (theor-linear-spectrum %) spec-map 0)]) candidates))]

    (map first
       (if (> (count sorted) trim-n)
         (take-while #(>= (last %) (last (or (nth sorted trim-n) (last sorted)))) sorted)
         sorted))))


(defn filter-too-heavy [candidates]
  (filter #(<= (apply + %) total-mass) candidates))


(defn find-leader [candidates]
  (cond
    (empty? candidates) nil
    (find-match candidates) (find-match candidates)
    :else (recur (trim-top (new-candidates candidates)))))


(clojure.string/join "-"
                     (find-leader '([])))
