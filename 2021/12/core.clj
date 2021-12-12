(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./ex3.txt")
;; (def FILENAME "./ex2.txt")

(defn map-kv [m f]
  (into {} (for [[k v] m] [k (f v)])))

(def state (->> (line-seq (io/reader FILENAME))
                (map #(str/split % #"-"))
                (reduce
                  (fn [acc [a b]]
                    (let [append (fn [old new]
                                   (if old (conj old new) #{new}))]
                      (-> acc
                       (update a append b)
                       (update b append a))))
                  {})))


;; (defn visitable-nodes [g visited start]
;;   (->> (g start)
;;        (filter #(if (= (str/upper-case %) %)
;;                   true
;;                   (zero? (visited %))))))

(defn visitable-nodes [visited nodes]
  (->> nodes
       (filter #(if (= (str/upper-case %) %)
                  true
                  (zero? (visited %))))))

(defn pred-excluding [node]
  (fn [visited nodes]
    (->> nodes
         (filter (fn [n]
                   (cond (= (str/upper-case n) n) true
                         (= n node)               (< (visited n) 2)
                         :else                    (zero? (visited n))))))))


(defn paths
  ([g pred] (paths g "start" "end" pred))
  ([g start stop pred] (paths g start stop pred (zipmap (keys g) (repeat 0))))
  ([g start stop pred visited]
   (let [next-candidates (pred visited (g start))
         visited'        (update visited start inc)]
     ;; (println "start stop visited" start stop visited)
     ;; (println "next-candidates" next-candidates)
     (cond (= start stop)           (list (list stop))
           (empty? next-candidates) '()
           :else (reduce
                  (fn [acc next-start]
                    (->> (paths g next-start stop pred visited')
                         (map #(conj % start))
                         (concat acc)))
                  '()
                  next-candidates)))))

(comment

  (seq nil)


  (set nil)

  state

  (conj #{:a} :a :b :c)

  (update {} :b conj :a)

  (paths state visitable-nodes)
  (paths {:a #{:b} :b #{:a}} :a :b)
  (paths {:a #{:b} :b #{:d :a} :d #{:b}} :a :d)
  (paths {:a #{:b} :b #{:a :d :c} :c #{:e} :d #{:e} :e #{:c :d}} :a :e)

  (zipmap (keys state) (repeat 0))

  (str/upper-case "a")

  (visitable-nodes {"a" #{"B"} "B" #{"a" "D" "c"} "c" #{"e"} "D" #{"e"} "e" #{"c" "D"}}
                   {"a" 1 "B" 10 "c" 0 "D" 3 "e" 0}
                   "B")

  (paths {"a" #{"b"} "b" #{"a" "d" "C"} "C" #{"e"} "d" #{"e"} "e" #{"C" "d"}} "a" "e")

  (paths state)

  (count (paths state visitable-nodes))

  (count
   (set
    (concat
     (paths state (pred-excluding "b"))
     (paths state (pred-excluding "c"))
     (paths state (pred-excluding "d")))))

  (let [doubled-nodes (filter #(and (= (str/lower-case %) %)
                                    ((complement #{"start" "end"}) %))
                              (keys state))]
    (->> doubled-nodes
         (mapcat #(paths state (pred-excluding %)))
         set
         count))


  nil)
