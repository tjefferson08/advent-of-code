(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def targets (->> (line-seq (io/reader FILENAME))
                  first
                  (re-find #"x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)")
                  (drop 1)
                  (map #(Integer/parseInt %))))

(def target-x (take 2 targets))
(def target-y (take 2 (drop 2 targets)))

;; 7,2
;; 0,0
;; 7, 2
;; 7 + 6, 2 + 1
;; 13 + 5, 3 + 0
;; 18 + 4, 3 - 1
;; 22 + 3, 2 - 2
;; 25 + 2, 0 - 3
;; 27 + 1, -3 - 4
;; 28 + 0, -7 - 5
;; 28 + 0, -12 - 6

(defn pos-seq
  ([vx vy]
   (cons {:v [vx vy] :pos [0 0]}
         (pos-seq vx vy vx vy)))
  ([vx vy x y]
   (let [vx' (max (dec vx) 0)
         vy' (dec vy)
         x'  (+ x vx')
         y'  (+ y vy')]
     (lazy-seq
      (cons {:v [vx vy] :pos [x y]} (pos-seq vx' vy' x' y'))))))

(defn constrain [p-seq]
  (->> p-seq
       (reduce
               (fn [acc {[vx vy] :v [x y] :pos :as el}]
                 (let [x-hit (<= (first target-x) x (second target-x))
                       y-hit (<= (first target-y) y (second target-y))
                       hit   (and x-hit y-hit)]
                   (if (or hit
                           (and (zero? vx) (< x (first target-x)))
                           (> x (second target-x))
                           (< y (first target-y)))
                     (reduced {:hit? hit
                               :seq  (conj acc el)
                               :v    (:v (first p-seq))
                               :max-y (reduce #(if (> %1 (second (:pos %2))) %1 (second (:pos %2))) 0 (conj acc el))})
                     (conj acc el))))
               [])))


(comment

  (take 10 (concat (range 7 0 -1) (repeat 0)))

  (take 12 (pos-seq 7 2))

  (let [{{b :b} :a} {:a {:b 10}}]
    [b])

  target-x

  target-y

  (constrain (pos-seq 7 2))

  (constrain (pos-seq 6 3))
  (constrain (pos-seq 6 9))
  (constrain (pos-seq 6 10))


  (take 10 (pos-seq 9 0))

  (constrain (pos-seq 9 0))

  (constrain (pos-seq 17 -4))

  (def attempts (for [vx (range 50)
                      vy (range 200)]
                    [[vx vy] (constrain (pos-seq vx vy))]))

  (count attempts)

  (filter #(:hit? (second %)) attempts)

  (count (filter #(:hit? (second %)) attempts))

  (time (->> (filter :hit? (for [vx (range 100)
                                 vy (range 200)]
                             (constrain (pos-seq vx vy))))
             (apply max-key :max-y)))

  (time (->> (filter :hit? (for [vx (range 130)
                                 vy (range -150 200)]
                             (constrain (pos-seq vx vy))))
             (map :v)
             count))

  target-x

  target-y

 (constrain (pos-seq 6 0))

 (take 250 (pos-seq 13 74))

 (constrain (pos-seq 15 149))



 nil)
