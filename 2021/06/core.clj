(ns core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def FILENAME "./input.txt")
;; (def FILENAME "./simple_input.txt")
;; (def FILENAME "./super_simple_input.txt")

(defn input->fish []
  (let [lines (line-seq (io/reader FILENAME))]
    (first (map (fn [line] (->> (str/split line #",")
                               (map #(Integer/parseInt %))))
            lines))))

(defn next-fish [fish]
   (if (zero? fish)
     [6 8]
     [(dec fish)]))

(defn tick [coll]
  (mapcat next-fish coll))

(defn tick-seq
   ([] (tick-seq (input->fish)))
   ([coll] (lazy-seq (cons coll (tick-seq (tick coll))))))


(defn -main []
  (println "count after 256" (count (last (take 257
                                           (iterate tick (input->fish)))))))

(defn pop-count [fish-start ticks]
  (int (Math/pow 2 (quot (- ticks fish-start) 8))))

(defn input->fish-counts []
  (frequencies (input->fish)))

(defn next-fish-counts [fish-counts]
  {0 (get fish-counts 1 0)
   1 (get fish-counts 2 0)
   2 (get fish-counts 3 0)
   3 (get fish-counts 4 0)
   4 (get fish-counts 5 0)
   5 (get fish-counts 6 0)
   6 (+ (get fish-counts 7 0) (get fish-counts 0 0))
   7 (get fish-counts 8 0)
   8 (get fish-counts 0 0)})

(comment
  (input->fish)

  (next-fish 0)
  (next-fish 6)

  (tick (tick (input->fish)))


  ;; part 2
  (def n 2)
  (last (take n
         (iterate tick (input->fish))))

  (assert (=
           (pop-count 0 1)
           2))

  (Math/pow
    2
    (quot 16 8))


  (assert (=
           (pop-count 1 1)
           1))


  (Math/pow 2 (quot n 8))

  (assert (=
           (next-fish-counts (input->fish-counts))
           {2 2, 0 1, 1 1, 3 1, 8 0, 7 0, 6 0, 5 0, 4 0}))

  (assert (=
           (next-fish-counts {0 3, 1 1, 8 2, 6 2})
           {0 1,
            1 0,
            2 0,
            3 0,
            4 0,
            5 2,
            6 3,
            7 2,
            8 3}))

 (reduce + (vals (last (take 257 (iterate next-fish-counts (input->fish-counts))))))

 nil)
