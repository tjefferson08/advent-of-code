(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(defn input->state []
  (let [lines (line-seq (io/reader FILENAME))]
    {:numbers (->> (str/split (first lines) #",")
                   (map #(Integer/parseInt %)))
     :boards (->> (drop 2 lines)
                  (partition-by #{""})
                  (filter #(not (= '("") %))) ;; keep non-empty "lines"
                  (map (fn [board]
                          (->> board
                               (map #(->> (re-seq #"\d+" %)
                                          (map #(Integer/parseInt %))))))))}))

(defn rows->columns [rows]
  (apply map vector rows))

(defn winning-board? [drawn rows]
  (let [columns (apply map vector rows)
        candidate-paths (concat rows columns)
        winning-paths (filter
                        (fn winning-path? [path]
                          (set/subset? (set path) (set drawn)))
                       candidate-paths)]
   (boolean (seq winning-paths))))

(defn first-winner [state]
  (loop [n 1]
   (let [drawn   (take n (:numbers state))
         winners (filter (partial winning-board? drawn) (:boards state))]
     (if (seq winners)
       {:board (first winners) :drawn drawn}
       (recur (inc n))))))

(defn all-winners [state]
  (loop [n 1
         candidate-boards (:boards state)
         winners-acc []]
   (let [drawn                        (take n (:numbers state))
         {winners true, losers false} (group-by (partial winning-board? drawn) candidate-boards)
         acc                          (concat winners-acc (map #(assoc {} :board % :drawn drawn) winners))]

     (println "acc" acc)

     (if (or
           (not (seq losers))
           (= n (count (:numbers state))))
       acc
      (recur (inc n) losers acc)))))

(defn score [drawn rows]
  (let [flat-nums (apply concat rows)
        unmarked-nums (set/difference (set flat-nums) (set drawn))]
    (* (reduce + unmarked-nums)
       (last drawn))))

(comment

  ;; Part 1
  (input->state)

  (drop 1 '(2 4 6))

  (partition 1 2 '(1 2 3 4 5 6))

  ;; NICE
  (re-seq #"\d+" " 1 23  4")

  (def board [[1 2 3] [4 5 6] [3 1 2]])

  (winning-board? '(4 1 3) board)

  (apply map vector board)


  (set/subset? #{1 2 3} #{ 5 4 3 2 1})

  (concat [[1 2 3] [4 5 6]] [[7 8 9]])

  (apply concat [[1 2 3] [4 5 6]])

  (concat [1] [1 2 3])


  (def state (input->state))
  (def winner (first-winner state))
  (score (:drawn winner) (:board winner))

  ;; Part 2
  (def last-winner (last (all-winners (input->state))))

  (:board last-winner)

  (score (:drawn last-winner) (:board last-winner))

  (set/difference #{1 2 3} #{1  2})


 nil)
