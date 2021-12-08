(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(defn input->crabs []
  (let [lines (line-seq (io/reader FILENAME))]
    (first (map (fn [line] (->> (str/split line #",")
                               (map #(Integer/parseInt %))))
            lines))))


(defn candidates [crabs]
  (range (apply min crabs)
         (apply max crabs)))

(defn distance [pos crab]
  (if (> pos crab)
    (- pos crab)
    (- crab pos)))

(defn distance-2 [pos crab]
  (let [d (distance pos crab)]
    (/ (* (inc d) d) 2)))

(defn fuel-cost [pos crabs]
  (reduce (fn [acc crab] (+ (distance pos crab) acc)) 0 crabs))

(defn fuel-cost-2 [pos crabs]
  (reduce (fn [acc crab] (+ (distance-2 pos crab) acc)) 0 crabs))

(comment
  (input->crabs)

  (distance 16 2)
  (distance 1 2)
  (distance 2 2)
  (distance 2 5)
  (distance 1 5)

  (fuel-cost 1 (input->crabs))
  (fuel-cost 2 (input->crabs))
  (fuel-cost 3 (input->crabs))
  (fuel-cost 10 (input->crabs))

  ;; Part 1, feels weird
  (let [crabs (input->crabs)
        positions (candidates crabs)]
    (as-> positions _
      (zipmap _ (map #(fuel-cost % crabs) _))
      (apply min-key val _)))

  ;; sneaky
  (distance-2 16 5)
  (distance-2 1 2)
  (distance-2 2 2)
  (distance-2 1 5)

  (let [crabs (input->crabs)
        positions (candidates crabs)]
    (as-> positions _
      (zipmap _ (map #(fuel-cost-2 % crabs) _))
      (apply min-key val _)))

  (fuel-cost 10 (input->crabs))


 nil)
