
(defn -movement-seq []
  (->> (line-seq (io/reader "./input.txt"))
       (map #(str/split % #" "))
       (map (fn [[dir mag]] [dir (Integer/parseInt mag)]))))

(defn -horizontal-pos [coll]
  (->> coll
    (filter #(#{"forward"} (first %)))
    (map second)
    (reduce +)))

(defn -depth [coll]
  (->> coll
       (reduce
           (fn [acc [dir mag]]
             (cond (= dir "up") (- acc mag)
                   (= dir "down") (+ acc mag)
                   :else acc))
         0)))

(defn -next-pos [{:keys [aim depth horizontal] :as pos} [cmd mag]]
  (cond (= cmd "up") (update pos :aim - mag)
        (= cmd "down") (update pos :aim + mag)
        (= cmd "forward") (-> pos
                              (update :horizontal + mag)
                              (update :depth + (*)))
        :else pos))


(comment
  (->> ["forward 2" "up 3" "down 1" "down 1"]
    (map #(str/split % #" "))
    (map (fn [[dir mag]] [dir (Integer/parseInt mag)])))

  (take 5 (-movement-seq))

  (-horizontal-pos (take 5 (-movement-seq)))

  (-depth (take 5 (-movement-seq)))

  (*
    (-horizontal-pos (-movement-seq))
    (-depth (-movement-seq)))


  ;; Part 2




 nil)
