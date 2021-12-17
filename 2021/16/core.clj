(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def hex-seq (->> (line-seq (io/reader FILENAME))))

(defn hex->bit-vec [hex-str]
  (->> (str/split hex-str #"")
       (map #(Integer/toString (Integer/parseInt % 16) 2))
       (mapcat #(format "%4s" %))
       (mapv {\1 1 \0 0 \space 0})))

(defn packet-meta [bit-vec]
  (let [[version type :as pair] (take 2 (partition 3 bit-vec))]
    {:version version :type type}))

(defn parse-value-packet [bit-vec]
  (let [meta (packet-meta bit-vec)
        raw  nil
        sub-packets []
        value (->> bit-vec
                   (drop 6)
                   (partition 5)
                   (reduce (fn [acc [leading-bit & remaining]]
                             (if (zero? leading-bit)
                               (reduced (conj acc remaining))
                               (conj acc remaining)))
                           []))]
    (merge meta {:raw nil, :sub-packets [], :value value})))

(defn value-packet? [bit-vec]
  (= [1 0 0] (:type (packet-meta bit-vec))))

(defn type-1-op-packet? [bit-vec]
  (and (not= [1 0 0] (:type (packet-meta bit-vec)))
       (= 1 (nth bit-vec 6))))

(defn type-2-op-packet? [bit-vec]
  (and (not= [1 0 0] (:type (packet-meta bit-vec)))
       (= 0 (nth bit-vec 6))))

;; (defn parse-op-packet [bit-vec]
;;   (->> bit-vec
;;        (drop 6)

(defn parse [bit-vec]
  (cond (value-packet? bit-vec) (parse-value-packet bit-vec)
        (type-1-op-packet? bit-vec) "total"
        (type-2-op-packet? bit-vec) "packet-count"
        :else (throw "hello")))



(comment

  (map identity (seq "D2FE28"))

  (count (hex->bit-vec "D2FE28"))

  (let [s (hex->bit-vec "D2FE28")]
    (println (packet-meta s))
    (println s))

  (value-packet? (hex->bit-vec "D2FE28"))
  (value-packet? (hex->bit-vec "38006F45291200"))

  (parse-value-packet (hex->bit-vec "D2FE28"))
  (parse (hex->bit-vec "D2FE28"))


  (type-1-op-packet? (hex->bit-vec "38006F45291200"))
  (type-2-op-packet? (hex->bit-vec "38006F45291200"))
  (value-packet? (hex->bit-vec "38006F45291200"))
  (parse (hex->bit-vec "38006F45291200"))


  nil)
