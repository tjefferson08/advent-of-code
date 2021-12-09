(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(def state (->> (line-seq (io/reader FILENAME))
                (map #(str/split % #"\s*\|\s*"))
                (map (fn [[patterns output]]
                       {:patterns (str/split patterns #"\s+")
                        :output   (str/split output #"\s+")}))))


(comment

  state

  (as-> state _
        (mapcat :output _)
        (group-by count _)
        (select-keys _ [2 4 3 7])
        (vals _)
        (apply concat _)
        (count _))


 nil)
