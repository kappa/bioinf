(defn log2 [n]
    (if (zero? n) 0 (/ (Math/log n) (Math/log 2))))

(defn entropy [seq]
  (- (apply + (map #(* (log2 %) %) seq))))

(prn (entropy [0.2 0 0.1 0.7]))
(prn (entropy [0.2 0 0.6 0.2]))
(prn (entropy [0 1 0 0]))
(prn (entropy [0 1 0 0]))
(prn (entropy [0 0.9 0 0.1]))
(prn (entropy [0 0.9 0 0.1]))
(prn (entropy [0.9 0.1 0 0]))
(prn (entropy [0.1 0 0.4 0.5]))
(prn (entropy [0.1 0 0.1 0.8]))
(prn (entropy [0.1 0 0.2 0.7]))
(prn (entropy [0.3 0 0.4 0.3]))
(prn (entropy [0 0 0.6 0.4]))

(prn (+ 1.1567796494470395
        1.3709505944546687
        -0.0
        -0.0
        0.4689955935892812
        0.4689955935892812
        0.4689955935892812
        1.360964047443681
        0.9219280948873623
        1.1567796494470395
        1.5709505944546684
        0.9709505944546686))
