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


(defn parse [chunks]
  (reduce
   (fn [[top & rest :as stack] c]
     (cond (#{"(" "{" "<" "["} c) (conj stack c)
           (= (delim-map c) top) rest
           :else (reduced {:expected (delim-map top) :actual c})))
   '()
   chunks))

(comment

  (parse ex)

  (map parse state)

  (->> state
       (map parse)
       (filter map?)
       (map :actual)
       (map points)
       (reduce +))



 nil)
