(def genome (read-line))
(def peptide (read-line))

(def codon-to-protein
(into {} (map #(clojure.string/split % #":" 2) (clojure.string/split-lines
"AAA:K
AAC:N
AAG:K
AAU:N
ACA:T
ACC:T
ACG:T
ACU:T
AGA:R
AGC:S
AGG:R
AGU:S
AUA:I
AUC:I
AUG:M
AUU:I
CAA:Q
CAC:H
CAG:Q
CAU:H
CCA:P
CCC:P
CCG:P
CCU:P
CGA:R
CGC:R
CGG:R
CGU:R
CUA:L
CUC:L
CUG:L
CUU:L
GAA:E
GAC:D
GAG:E
GAU:D
GCA:A
GCC:A
GCG:A
GCU:A
GGA:G
GGC:G
GGG:G
GGU:G
GUA:V
GUC:V
GUG:V
GUU:V
UAA:
UAC:Y
UAG:
UAU:Y
UCA:S
UCC:S
UCG:S
UCU:S
UGA:
UGC:C
UGG:W
UGU:C
UUA:L
UUC:F
UUG:L
UUU:F"))))

(defn rna-to-proteins [rna]
  (apply str (filter seq (map #(codon-to-protein (apply str %)) (partition 3 rna)))))

(defn dna-to-rna [dna]
  (map #(if (= % \T) \U %) dna))

(defn dna-to-peptide [dna]
  (rna-to-proteins (dna-to-rna dna)))

(defn rev-comp [dna]
  (reverse (map #(get {\A \T, \T \A, \G \C, \C \G} %) dna)))

(defn find-subs [genome pred subss]
  (cond
   (empty? genome) subss
   :else (recur (rest genome) pred
                (if (pred genome)
                  (cons (pred genome) subss)
                  subss))))

(defn encodes-peptide? [genome]
  (let [start (take (* 3 (count peptide)) genome)]
    (cond
      (= (dna-to-peptide start) peptide) start
      (= (dna-to-peptide (rev-comp start)) peptide) start
      :else nil)))


(println (clojure.string/join "\n" (map #(apply str %) (find-subs (seq genome) encodes-peptide? ()))))