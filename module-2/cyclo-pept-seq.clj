(def spectrum '(0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333))
(def peptide (seq "NQEL"))

(def spec-map (frequencies spectrum))
(def total-mass (last spectrum))

(def amino-mass (vals  {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \N 114, \D 115, \K 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   }))

(def amino-acids-dict {\G 57, \A 71, \S 87, \P 97, \V 99, \T 101, \C 103, \I 113, \L 113, \N 114, \D 115, \K 128, \Q 128, \E 129, \M 131, \H 137, \F 147, \R 156, \Y 163, \W 186
   })

(defn compute-amino-masses [peptide]
  (vec (map #(amino-acids-dict %) peptide)))


(def masses-masses (vec (concat (compute-amino-masses peptide) (compute-amino-masses peptide))))

(def N (count peptide))

(def product (for [start (range 0 N)
           len (range start (dec (+ start N)))]
       [start len]))

(defn theor-cycl-spectrum [peptide]
  (conj (vec (cons 0 (sort (map #(apply + %)
    (map (fn[pair] (subvec masses-masses (pair 0) (inc (pair 1))))
       product))))) (apply + (compute-amino-masses peptide)))
  )


(defn new-candidates [peptides]
  (for [peptide peptides
        new-amino amino-acids]
    (conj peptide new-amino)))

(defn theor-linear-spectrum [peptide]
  (for [start (range 0 (count peptide))
        end (range start (count peptide))]

    (apply + (subvec peptide start (inc end)))))

(defn peptide-consistent-new? [peptide spec-map]
  (let [one (first peptide)]
    (cond
      (empty? peptide) true
      (not (get spec-map one)) false
      (<= (get spec-map one) 0) false
      :else (recur (rest peptide) (update-in spec-map [one] dec)))))


(defn grep-consistent [candidates]
  (filter #(peptide-consistent-new? (theor-linear-spectrum %) spec-map) candidates))

(grep-consistent (map compute-amino-masses (map #(vec (seq %)) '("CTV"))))

(defn find-matched [candidates]
  (filter #(= (apply + %) total-mass) candidates))


(defn find-peptides [candidates found]
  (cond
    (empty? candidates) found
    :else (recur (grep-consistent (new-candidates candidates)) (concat found (find-matched candidates)))))


(defn output [found]
  (clojure.string/join " " (map #(clojure.string/join "-" %) found)))
