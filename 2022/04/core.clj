(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")


(deftype Range [start])

(defn input []
  (let [lines (line-seq (io/reader FILENAME))]
    (->> lines
      (map #(re-matches #"(\d+)-(\d+),(\d+)-(\d+)" %))
      (map rest)
      (map (fn [sub] (map #(Integer/parseInt %) sub))))))


(comment

  (->> (input)
       (filter (fn [[s1 e1 s2 e2]]
                   (or (<= s1 s2 e1) (<= s2 s1 e2))))
       count)

  (rest [1 2 3])

  (range 2 4)
  (range 1 6)

  nil)
