(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def template (->> (line-seq (io/reader FILENAME))
                   first
                   seq))

(def pairs (->> (line-seq (io/reader FILENAME))
                (drop 2)
                (map #(str/split % #" -> "))
                (map (fn [[pair insertion]] [(seq pair) (first (seq insertion))]))
                (into {})))

(defn apply-insertion [pairs template]
  (->> (partition 2 1 template)
       (map (fn [[first second :as pair]]
              (let [insertion (pairs pair)]
                (if insertion [first insertion second] first))))
       (map #(drop 1 %))
       (cons (take 1 template))
       flatten))

(defn next-pair-counts [pairs pair-counts]
  (let [count-updates (map
                       (fn [[k v]]
                         (let [insertion (pairs k)]
                           (if insertion { k 0
                                          [(first k) insertion] v
                                          [insertion (second k)] v})))
                       pair-counts)]
        (apply merge-with + count-updates)))

(defn char-frequencies [pair-frequencies]
  (->> pair-frequencies
       (map (fn [[[fst snd] ct]]
              {snd 0, fst ct}))
       (apply merge-with +)))

(comment

  (->> (partition 2 1 template)
       (mapcat (fn [[first second :as pair]]
              (let [insertion (pairs pair)]
                (if insertion (list (list first insertion) (list insertion second)) pair)))))

  (time
   (->>
    (partition 2 1 template)
    frequencies
    (iterate (partial next-pair-counts pairs))
    (take 41)
    last
    (sort-by second)
    char-frequencies
    ((fn [cf] (update cf (last template) inc)))
    (sort-by second)
    ((fn [cf-coll] (- (second (last cf-coll)) (second (first cf-coll)))))))




 nil)
