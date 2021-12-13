(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./ex3.txt")
;; (def FILENAME "./ex2.txt")

(def dots (->> (line-seq (io/reader FILENAME))
               (map #(first (re-seq #"(\d+),(\d+)" %)))
               (filter #(not (nil? %)))
               (map (fn [[full x y]] [(Integer/parseInt x) (Integer/parseInt y)]))
               set))

(def folds (->> (line-seq (io/reader FILENAME))
                (map #(first (re-seq #"fold along (x|y)=(\d+)" %)))
                (filter #(not (nil? %)))
                (map (fn [[full axis v]] [axis (Integer/parseInt v)]))))

(defn coords [dots]
  (for [x (range (inc (apply max (map first dots))))
        y (range (inc (apply max (map second dots))))]
    [x y]))

(defn map-kv [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn reflect [[x y] [axis v]]
  (if (= axis "x")
    [(min x (- v(- x v))) y]
    [x (min y (- v(- y v)))]))

(defn fold [dots [axis v]]
  (let [reflected-dots (set (map #(reflect % [axis v]) dots))
        all-dots       (set (concat dots reflected-dots))
        cs             (coords dots)
        cs'            (if (= axis "x")
                           (filter #(<= (first %) v) cs)
                           (filter #(<= (second %) v) cs))]
    ;; (println "axis" axis v)
    ;; (println "dots" dots)
    ;; (println "refl" reflected-dots)
    ;; (println "all" all-dots)
    ;; (println "coords" cs)
    ;; (println "coords'" cs')
    (->> cs'
         (filter all-dots))))

(def part2 (set (reduce (fn [acc [axis v]] (fold acc [axis v])) dots folds)))

(comment
  (re-seq #"(\d+),(\d+)" "100,101")

  (assert (= [5 4] (reflect [5 4] ["x" 6])))
  (assert (= [-1 4] (reflect [5 4] ["x" 2])))

  (assert (= [5 0] (reflect [5 4] ["y" 2])))

  (assert (= [5 4] (reflect [5 4] ["y" 6])))
  (assert (= [0 0] (reflect [0 14] ["y" 7])))

  (println (apply str (->> (coords part2)
                           (map (fn [[x y]]
                                 (str (if (zero? y) "\n" "")
                                      (if (part2 [x y]) "X" "-")))))))



                         ;; (fold  ["y" 3]))


 nil)
