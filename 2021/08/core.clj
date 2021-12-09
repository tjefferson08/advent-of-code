(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

;; # of segments
;; #: 0 1 2 3 4 5 6 7 8 9
;; N: 6 2 5 5 4 5 6 3 7 6

;;   0:      1:      2:      3:      4:
;;  aaaa    ....    aaaa    aaaa    ....
;; b    c  .    c  .    c  .    c  b    c
;; b    c  .    c  .    c  .    c  b    c
;; ....    ....    dddd    dddd    dddd
;; e    f  .    f  e    .  .    f  .    f
;; e    f  .    f  e    .  .    f  .    f
;; gggg    ....    gggg    gggg    ....

;;   5:      6:      7:      8:      9:
;; aaaa    aaaa    aaaa    aaaa    aaaa
;; b    .  b    .  .    c  b    c  b    c
;; b    .  b    .  .    c  b    c  b    c
;; dddd    dddd    ....    dddd    dddd
;; .    f  e    f  .    f  e    f  .    f
;; .    f  e    f  .    f  e    f  .    f
;; gggg    gggg    ....    gggg    gggg
(def seg-map {0 #{\a \b \c \e \f \g}
              1 #{\c \f}
              2 #{\a \c \d \e \g}
              3 #{\a \c \d \f \g}
              4 #{\b \c \d \f}
              5 #{\a \b \d \f \g}
              6 #{\a \b \d \e \f \g}
              7 #{\a \c \f}
              8 #{\a \b \c \d \e \f \g}
              9 #{\a \b \c \d \f \g}})

(def segments (seg-map 8))

(def state (->> (line-seq (io/reader FILENAME))
                (map #(str/split % #"\s*\|\s*"))
                (map (fn [[patterns output]]
                       {:patterns (str/split patterns #"\s+")
                        :output   (str/split output #"\s+")}))))

(def init (zipmap segments (repeat (set segments))))

(defn constrain [m k f pattern]
  (if (<= (count (m k)) 1)
    m
    (let [mp           (update m k f pattern)
          candidates   (mp k)
          mp-remaining (into {} (filter (fn [[_k _v]] (> (count _v) 0)) (dissoc mp k)))]

     (if (and (= 1 (count candidates))
              (set/subset? candidates (apply set/union (vals mp-remaining))))
       (reduce (fn [acc _k]
                (merge acc (constrain acc _k set/difference candidates)))
               mp
               (keys mp-remaining))
       mp))))

(defn constrain-1 [pattern mp]
  (-> mp
      (constrain \c set/intersection pattern)
      (constrain \f set/intersection pattern)

      (constrain \a set/difference pattern)
      (constrain \b set/difference pattern)
      (constrain \d set/difference pattern)
      (constrain \e set/difference pattern)
      (constrain \g set/difference pattern)))

(defn constrain-7 [pattern mp]
  (-> mp
      (constrain \c set/intersection pattern)
      (constrain \f set/intersection pattern)
      (constrain \a set/intersection pattern)

      (constrain \b set/difference pattern)
      (constrain \d set/difference pattern)
      (constrain \e set/difference pattern)
      (constrain \g set/difference pattern)))

(defn constrain-4 [pattern mp]
  (-> mp
      (constrain \c set/intersection pattern)
      (constrain \f set/intersection pattern)
      (constrain \b set/intersection pattern)
      (constrain \d set/intersection pattern)

      (constrain \a set/difference pattern)
      (constrain \e set/difference pattern)
      (constrain \g set/difference pattern)))

;; 6 segments
(defn constrain-0-6-9 [pattern mp]
  (-> mp
      (constrain \a set/intersection pattern)
      (constrain \b set/intersection pattern)
      (constrain \f set/intersection pattern)
      (constrain \g set/intersection pattern)))

;; 5 segments
(defn constrain-2-3-5 [pattern mp]
  (-> mp
    (constrain \a set/intersection pattern)
    (constrain \d set/intersection pattern)
    (constrain \g set/intersection pattern)))

(defn narrow-pattern [pattern m]
  (println "narrowing" pattern m)
  (let [f (case (count pattern)
            2 constrain-1
            3 constrain-7
            4 constrain-4
            5 constrain-2-3-5
            6 constrain-0-6-9
            7 (fn [p _m] _m))]
    (f pattern m)))

(defn mapping-for [{:keys [patterns output]}]
  (let [mapping (reduce (fn [acc pattern] (narrow-pattern (set pattern) acc))
                        init
                        patterns)]
    (set/map-invert (into {} (map (fn [[k v]] [k (first v)]) mapping)))))

(defn pattern->int [mapping pattern]
  ((set/map-invert seg-map) (set (map mapping pattern))))

(defn output-values [{:keys [patterns output] :as state}]
  (println state)
  (let [mapping (mapping-for state)
        int-seq (map #(pattern->int mapping %) output)]
   (Integer/parseInt (apply str int-seq))))

(comment

  state

  (as-> state _
        (mapcat :output _)
        (group-by count _)
        (select-keys _ [2 4 3 7])
        (vals _)
        (apply concat _)
        (count _))

  ;; Part 2
  (constrain-1 (set "ab") init)

  (constrain-7 (set "bag") init)

  (->> init
       (constrain-1 (set "ab"))
       (constrain-7 (set "bag")))

  (->> init (constrain-7 (set "dab"))
            (constrain-1 (set "ab"))
            (constrain-4 (set "eafb"))
            (constrain-0-6-9 (set "cagedb"))
            (constrain-0-6-9 (set "cdfgeb"))
            (constrain-0-6-9 (set "cefabd"))
            (constrain-2-3-5 (set "cdfbe"))
            (constrain-2-3-5 (set "gcdfa")))
            ;; (constrain-2-3-5 (set "fbcad"))
            ;; (constrain-2-3-5 (set "cdfeb"))
            ;; (constrain-2-3-5 (set "cdfeb"))
            ;; (constrain-2-3-5 (set "cdbaf")))

  (-> {\a #{\b \c} \b #{\b \d} \c #{\b \f}} (constrain \b set/intersection #{\b}))

  (println "****")
  (-> init (constrain \c set/intersection #{\a})
           (constrain \b set/intersection #{\a \c}))

  (->> init
    (narrow-pattern #{\a \b})
    (narrow-pattern #{\d \b \a}))

  (as-> state _
        (map output-values _)
        (reduce + _))

  ;; narrowing #{a b e f} {a #{d}, b #{c e f}, c #{a}, d #{c f}, e #{c e f g}, f #{b}, g #{c f}}

  (->> init
       (narrow-pattern (set "acedgfb"))
       (narrow-pattern (set "cdfbe"))
       (narrow-pattern (set "gcdfa"))
       (narrow-pattern (set "fbcad"))
       (narrow-pattern (set "dba"))
       (narrow-pattern (set "cefabd"))
       (narrow-pattern (set "cdfgeb"))
       (narrow-pattern (set "eafb")))

  (narrow-pattern
   (set "eafb")
   {\a #{\d},
    \b #{\c \e \f},
    \c #{\a},
    \d #{\c \f},
    \e #{\c \e \f \g},
    \f #{\b},
    \g #{\c \f}})


    ;; (constrain-2-3-5 (set "fbcad"))
    ;; (constrain-2-3-5 (set "cdfeb"))
    ;; (constrain-2-3-5 (set "cdfeb"))
    ;; (constrain-2-3-5 (set "cdbaf")))

  nil)
