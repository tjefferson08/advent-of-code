(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(defn input->segments []
  (let [lines (line-seq (io/reader FILENAME))]
    (map (fn [line] (->> (re-seq #"\d+" line)
                         (map #(Integer/parseInt %))
                         (partition 2)))
      lines)))


(defn horizontal? [[[x1 y1] [x2 y2]]]
  (= y1 y2))

(defn vertical? [[[x1 y1] [x2 y2]]]
  (= x1 x2))

;; points-for seg ((7 0) (7 4))
(defn points-for [seg]
  (let [[p1 p2] seg
        [x1 y1] p1
        [x2 y2] p2
        [start end] (if (<= x1 x2) [p1 p2] [p2 p1])
        [x-slope y-slope] (cond (horizontal? seg) [1 0]
                               (vertical? seg)   (if (> y1 y2) [0 -1] [0 1])
                               :else             [1 (/ (- y2 y1) (- x2 x1))])]
    (loop [p start
           ps [p]]
      (let [[x y]  p
            p-next [(+ x x-slope) (+ y y-slope)]]
        (if (= p end)
          (set ps)
          (recur p-next (conj ps p-next)))))))

(comment

 ;; Part 1
 (input->segments)


 (range 1 4)

 (range 2 -2 -1)

 (range 2 3)

 (range 2 6)


 (min 4 2 1)

 (points-for [[2 5] [2 2]])
 (points-for [[0 9] [5 9]])
 (points-for [[8 0] [0 8]])

 (assert (= (points-for [[2 5] [2 2]])
            #{[2 5] [2 3] [2 4] [2 2]}))

 (horizontal? [[1 2] [1 4]])
 (vertical? [[1 2] [1 4]])

 (->> (input->segments)
      (filter #(or (horizontal? %) (vertical? %)))
      (mapcat points-for))

 (->> (input->segments)
      (filter #(or (horizontal? %) (vertical? %)))
      (mapcat points-for)
      frequencies
      (filter (fn [[pt occur]] (> occur 1)))
      count)

 ;; part 2
 ;;

 (points-for [[2 5] [2 2]])
 (points-for [[0 9] [5 9]])
 (points-for [[1 1] [1 1]])
 (points-for [[2 0] [0 2]])
 (points-for [[2 4] [2 1]])

 (assert (= (points-for [[2 0] [0 2]])
            #{[2 0] [0 2] [1 1]}
            #{[2 5] [2 3] [2 4] [2 2]}))

 nil)
