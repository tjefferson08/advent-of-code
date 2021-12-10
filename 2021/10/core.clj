(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(def state (->> (line-seq (io/reader FILENAME))
                (map #(str/split % #""))))
(def ex (nth state 2))

(def delim-map {"(" ")", ")" "("
                "{" "}", "}" "{"
                "<" ">", ">" "<"
                "[" "]", "]" "["})

(def delims (set (keys delim-map)))
(def points {")" 3, "]" 57, "}" 1197, ">" 25137})
(def points-2 {")" 1, "]" 2, "}" 3, ">" 4})

(defn parse [chunks]
  (reduce
   (fn [[top & rest :as stack] c]
     (cond (#{"(" "{" "<" "["} c) (conj stack c)
           (= (delim-map c) top) rest
           :else (reduced {:expected (delim-map top) :actual c})))
   '()
   chunks))

(defn completion-score [coll])

(comment

  (parse ex)

  (map parse state)

  (->> state
       (map parse)
       (filter map?)
       (map :actual)
       (map points)
       (reduce +))

  (->> state
       (map parse)
       (filter sequential?)
       (map #(map delim-map %))
       (map #(reduce (fn [score c] (+ (* score 5) (points-2 c))) 0 %))
       sort
       ((fn [coll] (nth coll (/ (count coll) 2)))))




 nil)
