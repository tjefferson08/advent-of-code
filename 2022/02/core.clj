(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")

(defn input []
  (let [lines (line-seq (io/reader FILENAME))]
    lines))

;; (def shorthand {"A" :rock, "B" :paper, "C" :scissors, "X" :rock, "Y" :paper, "Z" :scissors})
(def shorthand {"A" :rock, "B" :paper, "C" :scissors, "X" :loss, "Y" :draw, "Z" :win})

(def scores {:loss 0, :draw 3, :win 6, :rock 1, :paper 2, :scissors 3})
(def defeats-map {:rock :scissors, :scissors :paper, :paper :rock})

(defn outcome [player opponent]
  (cond
      (= player opponent) :draw
      (= player (defeats-map opponent)) :loss
      :else :win))

(defn player-choice [opponent desired-outcome]
  (case desired-outcome
    :draw opponent
    :loss (defeats-map opponent)
    :win (opponent (clojure.set/map-invert defeats-map))))


(comment
  (->> (input)
       (map #(str/split % #" "))
       (map (fn [[opponent desired-outcome]]
              (let [o (shorthand opponent)
                    result (shorthand desired-outcome)]
                {:opponent o, :player (player-choice o result), :outcome result})))
       (map (fn [{:keys [player outcome]}]
              (+ (scores player) (scores outcome))))
       (reduce +))

  (player-choice :rock :loss)
  (player-choice :rock :win)
  (player-choice :rock :draw)

  (->> (input)
       (map #(str/split % #" "))
       (map (fn [[opponent player]]
              (let [o (shorthand opponent)
                    p (shorthand player)]
                {:opponent o, :player p, :outcome (outcome p o)})))
       (map (fn [{:keys [player outcome]}]
              (+ (scores player) (scores outcome))))
       (reduce +))




  (outcome :paper :scissors)
  (outcome :rock :scissors)
  (outcome :rock :paper)
  (outcome :rock :rock)

  (map #(str/split % #" ") input)

  (input)

  (->> (partition-by empty? (input))
       (filter #(not= % '("")))
       (map (fn [xs]
              (->> xs
                   (map #(Integer/parseInt %))
                   (reduce +))))
       sort
       reverse
       (take 3)
       (reduce +)))
