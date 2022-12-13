;; (def FILENAME "./input.txt")
(def FILENAME "./example.txt")

(defn lines [] (line-seq (io/reader FILENAME)))

(defn to-i [s] (Integer/parseInt s))

(defn parse [line-coll]
  (let [ints-on-line #(map to-i (re-seq #"\d+" (nth line-coll %)))
        monkey    (first (ints-on-line 0))
        items     (into [] (ints-on-line 1))
        operands  (drop 2 (re-seq #"\w+" (nth line-coll 2)))
        operation ({"+" +, "*" *} (re-find #"[+*]" (nth line-coll 2)))
        op        (fn [old] (apply operation (map #(if (= "old" %) old (to-i %)) operands)))
        divisor   (first (ints-on-line 3))
        target1   (first (ints-on-line 4))
        target2   (first (ints-on-line 5))
        target    #(if (zero? (mod % divisor)) target1 target2)]
    {:number monkey, :items items, :target target, :op op}))

(defn monkeys []
  (->> (lines)
     (partition-all 7)
     (map parse)
     ((fn [m-coll] (zipmap (map :number m-coll) m-coll)))))

(defn monkey-moves [{:keys [items op target]}]
  (->> items
       (map (fn [item] (quot (op item) 3)))
       (group-by target)))

(defn next-monkeys [m-map monkey]
  (pprint m-map)
  (let [moves  (monkey-moves monkey)
        mp     (assoc-in m-map [(monkey :number) :items] [])]
    (merge-with (fn [old new] (update old :items #(apply conj % new))) mp moves)))

(defn next-monkey-round [m-map]
  (pprint m-map)
  (reductions
    (fn [m i] (next-monkeys m (m i)))
    m-map
    (range (count m-map))))

(comment

  (keep-indexed #([%1 %2]) [2 4 6])

  (reductions + 8 [1 2 3])

  [(monkeys)]

  (->> (iterate (fn [round] (next-monkey-round (last round))) (next-monkey-round (monkeys)))
       (take 20)
       last)


  (merge-with  (fn [old new] (update old :items #(conj % new))) {:a {:items [1]}} {:a [2 3]})

  (count {:a 1, :b 2})

  (apply conj [1] [4 5 6])


  (def round1 (next-monkey-round (monkeys)))

  (last round1)

  (next-monkey-round (last round1))

  (def round2 (next-monkey-round (last round1)))
  (last round2)

  (next-monkeys (monkeys) ((monkeys) 0))

  (monkey-moves ((monkeys) 0))

  ((monkeys) 0)


  (update [1 2 3] 1 #(+ 2 %))

 ;; (quot ((:op m) 79) 3)

  (rem 10 4)


  nil)
