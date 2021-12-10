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


(defn neighbor-coords [x y]
  [[x (dec y)]
   [x (inc y)]
   [(inc x) y]
   [(dec x) y]])

(defn basin-for
  ([x y]
   (basin-for x y #{}))
  ([x y acc]
   (let [ns (neighbor-coords x y)
         basin-ns (->> (filter #(< (get-in state % 10) 9) ns)
                       (filter #(not (acc %))))]
     (if (seq basin-ns)
       (reduce (fn [_acc [bx by]] (basin-for bx by _acc))
               (into acc basin-ns)
               basin-ns)
       acc))))

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

  (basin-for 0 0)

  (basin-for 0 9)
  (seq [])

  (basin-for 0 9 #{[0 8]})

  state
  (count (basin-for 0 0))
  (count (basin-for 0 9))
  (count (basin-for 4 6))
  (count (basin-for 2 2))

  (->> (coords state)
       (map (fn [coord] [coord (apply neighbors coord)]))
       (filter (fn [[coord neighbors]] (every? #(< (get-in state coord) %) neighbors)))
       (map first)
       (map (fn [[x y]] (basin-for x y)))
       (map count)
       (sort >)
       (take 3)
       (reduce *))


  nil)
