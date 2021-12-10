(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(def state (->> (line-seq (io/reader FILENAME))
                (mapv (fn [line] (mapv #(Integer/parseInt %) (str/split line #""))))))

(defn neighbors [x y]
  (->> [[x (dec y)]
        [x (inc y)]
        [(inc x) y]
        [(dec x) y]]
       (map #(get-in state %))
       (filterv #(not (nil? %)))))

(defn coords [matrix]
  (for [x (range (count matrix))
        y (range (count (first matrix)))]
    [x y]))

(defn risk [coord]
  (+ 1 (get-in state coord)))

(comment

  state

  (neighbors 0 0)

  (neighbors 2 1)

  (every? #(> 10 %) [9 2 1])

  (->> (coords state)
       (map (fn [coord] [coord (apply neighbors coord)]))
       (filter (fn [[coord neighbors]] (every? #(< (get-in state coord) %) neighbors)))
       (map first)
       (map risk)
       (reduce +))


  nil)
