(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def matrix (->> (line-seq (io/reader FILENAME))
                 (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(def small-matrix (->> (line-seq (io/reader FILENAME))
                       (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(defn matrix-seq
  ([] (matrix-seq small-matrix))
  ([mtrx] (lazy-seq
           (cons mtrx (matrix-seq
                       (mapv (fn [row] (mapv #(if (= 9 %) 1 (inc %)) row)) mtrx))))))

;; Large matrix for part 2
(def matrix
  (->> (partition 100 1 (matrix-seq))
       (take 100)
       (mapv #(into [] (apply concat %)))
       (apply mapv concat)
       (mapv #(into [] %))))

;; (def GRID_SIZE (count matrix))
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

;; (def graph (reduce
;;             (fn [g coord] (assoc g coord
;;                                  {:adj (neighbors coord)
;;                                   :risk (get-in matrix (reverse coord))}))
;;             {}
;;             (coord-seq matrix)))

(defn traverse [matrix start end]
  (loop [unvisited (set (coord-seq matrix))
         distances (assoc (zipmap (coord-seq matrix) (repeat ##Inf)) start 0)]
    (if (unvisited end)
      (let [current             (apply min-key distances unvisited)
            ;; _                   (println "current" current)
            unvisited'          (disj unvisited current)
            nbors               (set (neighbors current))
            unvisited-neighbors (set/intersection nbors unvisited')
            tentative-distances (->> unvisited-neighbors
                                     (map (fn [nbor] [nbor (+ (get-in matrix [(second nbor) (first nbor)]) (distances current))]))
                                     (into {}))
            ;; _                   (println "tent" tentative-distances)
            distances' (merge-with min distances tentative-distances)]
            ;; _                   (println "un-prime" unvisited')]
        (recur unvisited'
               distances'))
     distances)))



(time
 (println ((traverse matrix [0 0] [499 499]) [499 499])))

;; (time (println "hi" ((traverse matrix [0 0] [99 99]) [99 99])))

(comment

  (count matrix)

  (count (first matrix))

  (neighbors [0 0])

  (neighbors [5 3])

  (coord-seq matrix)

  (get-in matrix [2 0])

  (traverse matrix [0 0] [0 0])

  ;; (set ((graph [0 1]) :adj))


  (map first (take 3 (matrix-seq)))







  nil)
