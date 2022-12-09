(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")
;; (def FILENAME "./example2.txt")

(defn lines [] (line-seq (io/reader FILENAME)))

(defn next-t-pos [[h-x h-y] [t-x t-y :as curr-t-pos]]
  (let [dx (- t-x h-x)
        adx (Math/abs dx)
        dy (- t-y h-y)
        ady (Math/abs dy)]
    (cond
          (= #{2} #{adx ady})   [((if (pos? dx) dec inc) t-x) ((if (pos? dy) dec inc) t-y)]
          (= #{2 1} #{adx ady}) [((if (pos? dx) dec inc) t-x) ((if (pos? dy) dec inc) t-y)]
          (< 1 adx)             [((if (pos? dx) dec inc) t-x) t-y]
          (< 1 ady)             [t-x ((if (pos? dy) dec inc) t-y)]
          :else curr-t-pos)))


(defn next-knot-coll [[h & knots :as knot-coll] move]
  (let [[h-x h-y] h
        [next-h-x next-h-y :as next-h] (case move
                                         "U" [h-x (inc h-y)]
                                         "D" [h-x (dec h-y)]
                                         "R" [(inc h-x) h-y]
                                         "L" [(dec h-x) h-y])]
    (reduce (fn [acc knot]
              (conj acc (next-t-pos (last acc) knot)))
            [next-h]
            knots)))


(comment
  (Math/abs -1)

  (let [moves (->> (lines)
                   (map #(str/split % #" "))
                   (map (fn [[dir n]] (repeat (Integer/parseInt n) dir)))
                   flatten)
        init-knots (into [] (repeat 10 [0 0]))
        states (reductions (fn [knots move] (next-knot-coll knots move)) init-knots moves)
        t-pos-coll (map last states)]
    (count (set t-pos-coll)))

  (next-knot-coll [[2 0] [1 0] [0 0]] "R")

  (next-t-pos [3 0] [1 0])


  (=                [[4 4] [4 3] [4 2] [3 2] [2 2] [1 1] [0 0] [0 0] [0 0] [0 0]]
    (next-knot-coll [[4 3] [4 2] [3 1] [2 1] [1 1] [0 0] [0 0] [0 0] [0 0] [0 0]] "U"))

  (next-knot-coll [[4 3] [4 2] [3 1] [2 1] [1 1] [0 0] [0 0] [0 0] [0 0] [0 0]] "U")


  nil)
