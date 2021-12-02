;; part 1
(defn count-depth-increases [coll]
  (->> coll
       (partition 2 1)
       (filter #(< (first %) (second %)))
       count))

(defn input-seq []
  (->> (line-seq (io/reader "./input.txt"))
       (map #(Integer/parseInt %))))

(defn count-sliding-window-increases [coll]
  (->> coll
       (partition 3 1)
       (map (fn [[x y z]] (+ x y z)))
       (count-depth-increases)))



(comment
  (println "HI")

  (assert (= 3 (count-depth-increases '(1 2 3 2 2 4 0))))

  (count-depth-increases (input-seq))

  (count-sliding-window-increases (input-seq))

  (partition 3 1 '(1 2 3 4 5 6))

  (count-sliding-window-increases '(1 2 3 2 0 1 1 2))

  (->> (line-seq (io/reader "./input.txt")
        (map #(Integer/parseInt %))
        (partition 2 1)
        (filter #(< (first %) (second %)))
        count))

  nil)
