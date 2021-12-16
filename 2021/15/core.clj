(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def matrix (->> (line-seq (io/reader FILENAME))
                 (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(def GRID_SIZE (count matrix))

(defn neighbors [[x y :as p]]
  (filterv (fn [[x' y']] (and (<= 0 x' (dec GRID_SIZE))
                             (<= 0 y' (dec GRID_SIZE))))
           [[(dec x) y]
            [(inc x) y]
            [x (inc y)]
            [x (dec y)]]))

(defn coord-seq [matrix]
  (for [x (range GRID_SIZE)
        y (range GRID_SIZE)]
    [x y]))

(def graph (reduce
            (fn [g coord] (assoc g coord
                                 {:adj (neighbors coord)
                                  :risk (get-in matrix (reverse coord))}))
            {}
            (coord-seq matrix)))

(defn traverse [graph start end]
  (loop [unvisited (set (coord-seq matrix))
         current start
         distances (assoc (zipmap (keys graph) (repeat ##Inf)) start 0)]
    (let [unvisited-neighbors (set/intersection (set (:adj (graph current))) unvisited)
          tentative-distances (into {} (map (fn [n] [n (+ (:risk (graph n)) (distances current))]) unvisited-neighbors))]
      (if (unvisited end)
        (recur (disj unvisited current)
               (first (sort-by distances unvisited))
               (merge-with min distances tentative-distances))
        distances))))






(comment

  (count matrix)

  (count (first matrix))

  (neighbors [0 0])

  (neighbors [5 3])

  (coord-seq matrix)

  (get-in matrix [2 0])

  (traverse graph [0 0] [99 99])

  (set ((graph [0 1]) :adj))


  nil)
