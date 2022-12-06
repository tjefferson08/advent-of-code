(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")


(defn lines [] (line-seq (io/reader FILENAME)))

(defn crates []
  (->> (lines)
       (filter #(re-find #"\[[A-Z]\]" %))
       (map (fn [line]
              (->> line
                (partition-all 4)
                (map second)
                (partition 1))))
       (map #(zipmap (range) %))
       (apply merge-with concat)
       (map (fn [[k v]] [(inc k) (filter #(not= \space %) v)]))
       (into {})))

(defn moves []
  (->> (lines)
      (map #(re-matches #"move (\d+) from (\d+) to (\d+)" %))
      (filter some?)
      (map rest)
      (clojure.walk/postwalk #(if (string? %) (Integer/parseInt %) %))))

(defn apply-move [crates [n from to]]
  (let [[removed next-from-stack] (split-at n (crates from))
        to-stack   (crates to)]
    (-> crates
        (assoc from next-from-stack
               to (concat removed to-stack)))))

(comment
  (moves)
  (crates)

  (reduce apply-move (crates) '((1 2 1) (3 1 3)))

  (reduce apply-move (crates) (moves))

  (moves)

  (apply-move {1 '(1 3 4), 2 '(2)} [2 1 2])

  (into {} ['(1 2) '(3 4)])

  (->> (lines)
      (map #(re-matches #"move (\d+) from (\d+) to (\d+)" %))
      (filter some?)
      (map rest)
      (clojure.walk/postwalk #(if (string? %) (Integer/parseInt %) %)))


  ((partial apply vec) 1 2)

 nil)
