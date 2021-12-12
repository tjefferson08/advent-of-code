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
  ([g] (paths g "start" "end" #{}))
  ([g start stop visited]
   (println start stop visited)
   (let [next-candidates (set/difference (g start) visited)
         visited'        (conj visited start)]
     (cond (= start stop)           (list (list stop))
           (empty? next-candidates) '()
           :else (reduce
                  (fn [acc next-start]
                    (->> (paths g next-start stop visited')
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
  (paths {:a #{:b} :b #{:a}} :a :b #{})
  (paths {:a #{:b} :b #{:d :a} :d #{:b}} :a :d #{})
  (paths {:a #{:b} :b #{:a :d :c} :c #{:e} :d #{:e}} :a :e #{})

  nil)
