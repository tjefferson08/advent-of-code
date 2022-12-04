(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")

(defn input []
  (let [lines (line-seq (io/reader FILENAME))]
    lines))

(def priorities
  (zipmap
   (seq "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
   (drop 1 (range))))


(comment
  (->> (input)
       (partition 3)
       (mapcat (fn [[first second third]] (clojure.set/intersection (set first) (set second) (set third))))
       (map priorities)
       (reduce +))


  (priorities \b)

  (->> (input)
       (map #(split-at (/ (count %) 2) %))
       (mapcat (fn [[first second]] (clojure.set/intersection (set first) (set second))))
       (map priorities)
       (reduce +))


  (int \p)
  (- (int \p) (int \a))
  (- (int \L) (int \a))

  (int \t)


  nil)
