;; (def FILENAME "./input.txt")
(def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(defn map-kv [m f]
  (into {} (for [[k v] m] [k (f v)])))

(def state (->> (line-seq (io/reader FILENAME))
                (map #(str/split % #"-"))
                (reduce
                  (fn [acc [a b]]
                     (let [append (fn [old new] (if old (conj old new) #{}))]
                       (-> acc
                        (update a append b)
                        (update b append a))))
                  {})))


(defn paths
  ([g] (paths g "start" "end"))
  ([g start stop] (paths g start stop (zipmap (keys g) (repeat 0))))
  ([g start stop visited]
   (let [next-candidates (set/difference (g start) (set (filter #(pos? (visited %)) (keys visited))))
         visited'        (update visited start inc)]
     (println "start stop visited" start stop visited)
     (println "next-candidates, visited'" next-candidates visited')
     (cond (= start stop)           (list (list stop))
           (empty? next-candidates) '()
           :else (reduce
                  (fn [acc next-start]
                    (->> (paths g next-start stop visited')
                         ((fn [el] (println "reducing" el) el))
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

  (paths state)
  (paths {:a #{:b} :b #{:a}} :a :b)
  (paths {:a #{:b} :b #{:d :a} :d #{:b}} :a :d)
  (paths {:a #{:b} :b #{:a :d :c} :c #{:e} :d #{:e} :e #{:c :d}} :a :e)

  (zipmap (keys state) (repeat 0))

  nil)
