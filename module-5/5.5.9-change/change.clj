(def money 16933)
(def coins [16 5 3 1])

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))

(defn-memo compute-min-coins [money coins num-coins]
  ;;(print ".")
  (cond
    (zero? money) num-coins
    :else (apply min (map #(compute-min-coins (- money %) coins (inc num-coins)) (filter #(<= % money) coins)))))

(defn compute-change-vec [coins coin-nums-vec]
  ;;(prn coin-nums-vec)
  (let [m (count coin-nums-vec)]
    (cond
      (zero? m) [0]
      :else (conj coin-nums-vec (apply min (map #(inc (coin-nums-vec (- m %))) (filter #(<= % m) coins)))))))

(println (get (nth (iterate (partial compute-change-vec coins) []) (inc money)) money))
