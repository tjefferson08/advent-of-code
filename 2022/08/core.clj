(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")

(defn lines [] (line-seq (io/reader FILENAME)))

(defn trees []
  (->> (lines)
       (map seq)
       (map (fn [row] (map #(- (int %) (int \0)) row)))
       (keep-indexed (fn [j row]
                       (keep-indexed (fn [i tree] {[i j] tree}) row)))
       flatten
       (apply merge)))

(defn visible? [[tree-x tree-y] trees grid-size]
  (let [tree (trees [tree-x tree-y])
        line-of-sight? (fn [path] (every? #(> tree %) (map trees path)))
        up    (for [x [tree-x] y (range tree-y)]                 [x y])
        down  (for [x [tree-x] y (range (inc tree-y) grid-size)] [x y])
        left  (for [x (range tree-x) y [tree-y]]                 [x y])
        right (for [x (range (inc tree-x) grid-size) y [tree-y]] [x y])]
   (or (line-of-sight? up)
       (line-of-sight? down)
       (line-of-sight? left)
       (line-of-sight? right))))

(defn scenic-score [[tree-x tree-y] tree-map grid-size]
  (let [tree (tree-map [tree-x tree-y])
        up    (for [x [tree-x] y (range (dec tree-y) -1 -1)]     [x y])
        down  (for [x [tree-x] y (range (inc tree-y) grid-size)] [x y])
        left  (for [x (range (dec tree-x) -1 -1) y [tree-y]]     [x y])
        right (for [x (range (inc tree-x) grid-size) y [tree-y]] [x y])
        visible-trees (fn [tree-path]
                        (reduce (fn [acc t] (if (> tree t)
                                              (conj acc t)
                                              (reduced (conj acc t))))
                          []
                          tree-path))
        up-trees (map tree-map up)
        down-trees (map tree-map down)
        left-trees (map tree-map left)
        right-trees (map tree-map right)]
    (reduce * [(count (visible-trees up-trees))
               (count (visible-trees down-trees))
               (count (visible-trees left-trees))
               (count (visible-trees right-trees))])))


(comment

  (trees)

  (scenic-score [2 3] (trees) 5)

  (let [tree-map      (trees)
        grid-size (inc (apply max (map first (keys tree-map))))
        scores    (map (fn [[pos _tree]] (scenic-score pos tree-map grid-size)) tree-map)]
   (apply max scores))

  (let [tree-map      (trees)
        grid-size (inc (apply max (map first (keys tree-map))))
        visible-trees (filter (fn [[pos _tree]] (visible? pos tree-map grid-size)) tree-map)]
   (count visible-trees))


  (let [tree-pos [1 3]
        up (for [x (take 1 tree-pos)
                 y (range (second tree-pos))]
             [x y])
        down (for [x [(first tree-pos)] y (range (inc (second tree-pos)) 6)]
               [x y])]
    down)

  nil)
