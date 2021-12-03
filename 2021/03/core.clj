
(defn input->bit-colls []
  (->> (line-seq (io/reader "./input.txt"))
    (map #(str/split % #""))
    (map (fn [digits] (map #(Integer/parseInt %) digits)))))

(defn bit-counts [bit-colls]
  (apply map + bit-colls))

(comment

  ;; Part 1
  (let [colls (->> (line-seq (io/reader "./input.txt"))
                   (take 3)
                   (map #(str/split % #""))
                   (map (fn [digits] (map #(Integer/parseInt %) digits))))]
    (apply map + colls))

  (take 5 (input->bit-colls))



  nil)
