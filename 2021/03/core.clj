(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(defn input->bit-colls []
  (->> (line-seq (io/reader FILENAME))
    (map #(str/split % #""))
    (map (fn [digits] (map #(Integer/parseInt %) digits)))))

(def INPUT_SIZE 1000)

(defn bit-counts [bit-colls]
  (apply map + bit-colls))

(defn gamma-rate [bit-colls]
  (->> (bit-counts bit-colls)
       (map #(if (>= % (/ (count bit-colls) 2))
               1
               0))))

;; this is just the inverse of gamma-rate
(defn epsilon-rate [bit-colls]
  (->> (bit-counts bit-colls)
       (map #(if (>= % (/ (count bit-colls) 2))
               0
               1))))

(defn bit-coll->dec [coll]
  (Integer/parseInt (str/join "" coll ) 2))

;; TODO recalc gamma-col with new data whoops
(defn oxygen-generator [bit-colls]
  (loop [remaining-colls bit-colls
         gamma-idx 0]
    (let [gamma-coll (gamma-rate remaining-colls)
          narrowed-colls (filter
                           #(= (nth % gamma-idx) (nth gamma-coll gamma-idx))
                           remaining-colls)]
      ;; (println "filtered by " (nth gamma-coll gamma-idx))
      ;; (println "narrowed:  " (count narrowed-colls) narrowed-colls)
      (if (= 1 (count narrowed-colls))
          (first narrowed-colls)
          (recur narrowed-colls (inc gamma-idx))))))

(defn co2-scrubber [bit-colls]
  (loop [remaining-colls bit-colls
         eps-idx 0]
    (let [eps-coll (epsilon-rate remaining-colls)
          narrowed-colls (filter
                           #(= (nth % eps-idx) (nth eps-coll eps-idx))
                           remaining-colls)]
      (println "filtered by " (nth eps-coll eps-idx))
      (println "narrowed:  " (count narrowed-colls) narrowed-colls)
      (if (= 1 (count narrowed-colls))
          (first narrowed-colls)
          (recur narrowed-colls (inc eps-idx))))))


(comment

  ;; Part 1
  (let [colls (->> (line-seq (io/reader "./input.txt"))
                   (take 3)
                   (map #(str/split % #""))
                   (map (fn [digits] (map #(Integer/parseInt %) digits))))]
    (apply map + colls))

  (take 5 (input->bit-colls))

  (bit-counts (input->bit-colls))


  (->>
   (take 3 (input->bit-colls))
   (gamma-rate)
   (bit-coll->dec))

  (->>
   (take 3 (input->bit-colls))
   epsilon-rate)

  (let [gamma (gamma-rate (input->bit-colls))
        eps   (epsilon-rate (input->bit-colls))]
   (* (bit-coll->dec gamma) (bit-coll->dec eps)))

  ;; Part 2
  ;;
  (assert (= (gamma-rate (input->bit-colls))
             [1 0 1 1 0]))

  (*
    (oxygen-generator (input->bit-colls))
    (co2-scrubber (input->bit-colls)))

 nil)
