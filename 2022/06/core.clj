(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")
(def EXAMPLE "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def EXAMPLE2 "bvwbjplbgvbhsrlpgdmjqwftvncz")


(defn lines [] (line-seq (io/reader FILENAME)))

(defn find [n [group & remaining]]
  (let [l (count group)]
    (if (= l (count (set group)))
      (+ n l -1)
      (recur (inc n) remaining))))

(comment

  (partition 14 1 EXAMPLE)

  (find 1 (partition 14 1 EXAMPLE))
  (find 1 (partition 14 1 EXAMPLE2))
  (find 1 (partition 14 1 (slurp FILENAME)))




 nil)
