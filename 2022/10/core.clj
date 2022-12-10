(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")
;; (def FILENAME "./example2.txt")

(defn lines [] (line-seq (io/reader FILENAME)))

(defn instructions []
  (->> (lines)
    (map #(str/split % #" "))
    (map (fn [[op arg]] [op (and arg (Integer/parseInt arg))]))))

(defn states [clock x [op arg :as inst]]
  (case op
    "noop" [[(inc clock) x x]]
    "addx" [[(inc clock) x x] [(inc (inc clock)) x (+ x arg)]]
    (throw)))

(defn cycles
  ([] (cycles 0 1 (instructions) []))
  ([clk x [inst & rem] cycle-coll]
   (if (nil? inst) cycle-coll
     (let [inst-cycles          (states clk x inst)
           next-cycle-coll      (apply conj cycle-coll inst-cycles)
           [next-clk _x next-x] (last inst-cycles)]
        (cycles next-clk next-x rem next-cycle-coll)))))


(comment
  (->> (cycles)
       (map (fn [[clk x :as cycle]] (if (#{x (inc x) (+ 2 x)} (mod clk 40)) "#" ".")))
       (partition 40)
       (map (partial apply str))
       (str/join "\n")
       print)

;; slightly off but close enough
;; "###...##..####.####.#..#.#..#.###..#..##"
;; "#..#.#..#....#.#....#..#.#..#.#..#.#.#.#"
;; "#..#.#......#..###..####.#..#.#..#.##..."
;; "###..#.##..#...#....#..#.#..#.###..#.#.#"
;; "#.#..#..#.#....#....#..#.#..#.#.#..#.#.#"
;; "#..#..###.####.####.#..#..##..#..#.#..#."

  (->> (cycles)
       (drop 19)
       (take-nth 40)
       (map (fn [[clk x]] (* clk x)))
       (reduce +))





  (cycles)


  (instructions)

  (states 0 1 ["noop"])
  (states 1 1 ["addx" 3])
  (states 3 4 ["addx" -5])


  (->> (lines)
       (map #(str/split % #" "))
       (map (fn [[inst x]] [inst (and x (Integer/parseInt x))])))


  nil)
