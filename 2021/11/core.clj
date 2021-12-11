;; (def FILENAME "./input.txt")
(def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(def state (->> (line-seq (io/reader FILENAME))
                (map-indexed
                 (fn [x l] (map-indexed
                            (fn [y el] [[x y] (Integer/parseInt el)])
                            (str/split l #""))))
                (apply concat)
                (into {})))

(def GRID_SIZE 10)

(defn map-kv [m f]
  (into {} (for [[k v] m] [k (f v)])))

;; TODO: use clojure.walk/walk ?
(defn next [s]
  (let [step1 (map-kv s inc)
        step2 (flash step1)]
    step2))


(defn flash
  ([s] (flash s #{}))
  ([s flashed]
   (let [flashers     (->> (apply dissoc s flashed)
                           (filter (fn [[coord val]] (> val 9)))
                           (map first)
                           (into #{}))
         adjacents    (mapcat neighbors flashers)
         increments   (frequencies adjacents)
         s'           (merge-with + s increments)
         next-flashers (as-> s' _
                         (filter (fn [[coord val]] (> val 9)) _)
                         (map first _)
                         (into #{} _)
                         (set/difference _ flashers flashed))]
    (if (seq next-flashers)
      (flash s' (set/union flashed flashers))
      (into {} (map (fn [[k v]] [k (if (> v 9) 0 v)]) s'))))))


(defn neighbors [[x y]]
  (for [nx [(dec x) x (inc x)]
        ny [(dec y) y (inc y)]
        :when (and (< -1 nx GRID_SIZE)
                   (< -1 ny GRID_SIZE)
                   (not= [nx ny] [x y]))]
   [nx ny]))

(comment

  state

  (sort-by first (next (next state)))

  (coords)

  (flash (next state))

  (sort-by first (flash (next (next state))))

  (->> (iterate next state)
       (take 101)
       (map (fn [s] (count (filter zero? (vals s)))))
       (reduce +))

  (->> (iterate next state)
       (map (fn [s] (count (filter zero? (vals s)))))
       (take-while #(< % 100))
       count)

  nil)
