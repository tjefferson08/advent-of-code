(def FILENAME "./input.txt")

(defn input []
  (let [lines (line-seq (io/reader FILENAME))]
    lines))

(comment

  (->> (partition-by empty? (input))
       (filter #(not= % '("")))
       (map (fn [xs]
              (->> xs
                   (map #(Integer/parseInt %))
                   (reduce +))))
       sort
       reverse
       (take 3)
       (reduce +))





  )
