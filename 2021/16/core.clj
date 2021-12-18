(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")

(def input (->> (first (line-seq (io/reader FILENAME)))))

(defn hex->bit-vec [hex-str]
  (->> (str/split hex-str #"")
       (map #(Integer/toString (Integer/parseInt % 16) 2))
       (mapcat #(format "%4s" %))
       (mapv {\1 1 \0 0 \space 0})))

(defn bit-vec->dec [bit-vec]
  (Long/parseLong (apply str bit-vec) 2))
;; (defn bit-vec->dec [bit-vec]
;;   (Integer/parseInt (apply str bit-vec) 2))

(defn packet-meta [bit-vec]
  (let [[version type :as pair] (take 2 (partition 3 bit-vec))]
    {:version (bit-vec->dec version) :type type}))

(defn parse-value-packet [bit-vec]
  (let [meta        (packet-meta bit-vec)
        sub-packets []
        val-chunks  (->> bit-vec
                         (drop 6)
                         (partition 5)
                         (reduce (fn [acc [leading-bit & remaining]]
                                   (if (zero? leading-bit)
                                     (reduced (conj acc remaining))
                                     (conj acc remaining)))
                                 []))
        value       (apply concat val-chunks)
        packet-ln   (+ 6 (* 5 (count val-chunks)))
        raw         (subvec bit-vec 0 packet-ln)]
    {:meta meta :raw raw, :ln packet-ln, :sub-packets [], :value (bit-vec->dec value)}))

(defn value-packet? [bit-vec]
  (= [1 0 0] (:type (packet-meta bit-vec))))

(defn type-1-op-packet? [bit-vec]
  (and (not= [1 0 0] (:type (packet-meta bit-vec)))
       (> (count bit-vec) 7)
       (= 1 (nth bit-vec 6))))

(defn type-0-op-packet? [bit-vec]
  (and (not= [1 0 0] (:type (packet-meta bit-vec)))
       (> (count bit-vec) 7)
       (= 0 (nth bit-vec 6))))

(defn parse [bit-vec]
  (println "bv" (packet-meta bit-vec) (apply str bit-vec))
  (cond
    (< (count bit-vec) 10) []
    (value-packet? bit-vec) (parse-value-packet bit-vec)
    (type-0-op-packet? bit-vec) (let [sub-packet-len (bit-vec->dec (take 15 (drop 7 bit-vec)))
                                      remaining      (subvec bit-vec (+ 7 15) (+ 7 15 sub-packet-len))
                                      sub-packets    (loop [rm          remaining
                                                            parsed-bits 0
                                                            acc         []]
                                                       (if (>= parsed-bits sub-packet-len)
                                                         acc
                                                         (let [pkt (parse rm)]
                                                           (recur (subvec rm (:ln pkt))
                                                                  (+ parsed-bits (:ln pkt))
                                                                  (conj acc pkt)))))]
                                  {:meta           (packet-meta bit-vec)
                                   :length-id      0
                                   ;; :remaining      remaining
                                   :sub-packet-len sub-packet-len
                                   :ln             (reduce + 22 (map :ln sub-packets))
                                   :sub-packets    sub-packets})


    (type-1-op-packet? bit-vec) (let [sub-packet-count (bit-vec->dec (take 11 (drop 7 bit-vec)))
                                      remaining        (subvec bit-vec (+ 7 11))
                                      sub-packets      (loop [rm             remaining
                                                              parsed-packets 0
                                                              acc            []]
                                                         (if (>= parsed-packets sub-packet-count)
                                                           acc
                                                           (let [pkt (parse rm)]
                                                             (recur (subvec rm (:ln pkt))
                                                                    (inc parsed-packets)
                                                                    (conj acc pkt)))))]
                                  {:meta             (packet-meta bit-vec)
                                   :length-id        1
                                   ;; :remaining        remaining
                                   :sub-packet-count sub-packet-count
                                   :ln               (reduce + 18 (map :ln sub-packets))
                                   :sub-packets      sub-packets})
    :else (throw "hello")))

;; (parse (hex->bit-vec "8A004A801A8002F478"))
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
  (type-0-op-packet? (hex->bit-vec "38006F45291200"))
  (value-packet? (hex->bit-vec "38006F45291200"))

  (partition 4 (hex->bit-vec "38006F45291200"))

  ;; Type 0
  (parse (hex->bit-vec "38006F45291200"))

  (subvec [1 2 3 4] 1 3)

  (bit-vec->dec [0  0 1 1])

  ;; Type 1
  (parse (hex->bit-vec "EE00D40C823060"))

  (parse (hex->bit-vec "8A004A801A8002F478"))

  (parse (hex->bit-vec "620080001611562C8802118E34"))

  (parse (hex->bit-vec "C0015000016115A2E0802F182340"))
  (parse (hex->bit-vec "A0016C880162017C3686B18A3D4780"))

  (hex->bit-vec input)

  (->> (parse (hex->bit-vec input))
       (tree-seq map? :sub-packets)
       (map #(get-in % [:meta :version]))
       (reduce +))




 nil)
