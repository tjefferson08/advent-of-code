(ns core
  (:require [clojure.java.io :as io]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [clojure.set :as set]))

(def matrix-ex-p1 (->> (line-seq (io/reader "./simple_input.txt"))
                       (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(def matrix-p1 (->> (line-seq (io/reader "./input.txt"))
                    (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(def matrix-joseph (->> (line-seq (io/reader "./joseph_input.txt"))
                        (mapv (fn [l] (mapv #(Integer/parseInt %) (str/split l #""))))))

(defn matrix-seq
  ([mtrx] (lazy-seq
           (cons mtrx (matrix-seq
                       (mapv (fn [row] (mapv #(if (= 9 %) 1 (inc %)) row)) mtrx))))))

;; Large matrix for part 2
(def matrix-ex-p2
  (->> (partition 5 1 (matrix-seq matrix-ex-p1))
       (take 5)
       (mapv #(into [] (apply concat %)))
       (apply mapv concat)
       (mapv #(into [] %))))

(def matrix-p2
  (->> (partition 5 1 (matrix-seq matrix-p1))
       (take 5)
       (mapv #(into [] (apply concat %)))
       (apply mapv concat)
       (mapv #(into [] %))))

(def matrix-joseph-p2
  (->> (partition 5 1 (matrix-seq matrix-joseph))
       (take 5)
       (mapv #(into [] (apply concat %)))
       (apply mapv concat)
       (mapv #(into [] %))))

(defn neighbors [matrix [x y :as p]]
  (filterv (fn [[x' y']] (and (< -1 x' (count matrix))
                              (< -1 y' (count matrix))))
           [[(dec x) y]
            [(inc x) y]
            [x (inc y)]
            [x (dec y)]]))

(defn coord-seq [matrix]
  (for [x (range (count matrix))
        y (range (count matrix))]
    [x y]))

;; (def graph (reduce
;;             (fn [g coord] (assoc g coord
;;                                  {:adj (neighbors coord)
;;                                   :risk (get-in matrix (reverse coord))}))
;;             {}
;;             (coord-seq matrix)))

(defn traverse [matrix start end]
  (loop [unvisited (priority-map start (get-in matrix (reverse start)))
         distances (assoc (zipmap (coord-seq matrix) (repeat ##Inf)) start 0)]
    ;; (println "invisited" unvisited (empty? unvisited))
    ;; (println (count unvisited))
    (if (empty? unvisited)
      (distances end)
      (let [current             (first (peek unvisited))
            ;; _                   (println "current" current)
            unvisited'          (pop unvisited)
            ;; _                   (println "un'" unvisited')
            ;; _                   (println "*****" (neighbors current))
            tentative-distances (->> (neighbors matrix current)
                                     (filter (complement unvisited'))
                                     (map (fn [nbor] [nbor (+ (get-in matrix [(second nbor) (first nbor)]) (distances current))]))
                                     (into {}))
            min-distances       (filter (fn [[k v]] (< v (distances k))) tentative-distances)
            ;; _                   (println "new-mins" min-distances)
            distances'          (merge distances min-distances)]
            ;; _                   (println "un-prime" unvisited')]
        (recur (into unvisited' min-distances)
               distances')))))



;; (time
;;  (println ((traverse matrix [0 0] [499 499]) [499 499])))

;; (time (println "hi" ((traverse matrix [0 0] [99 99]) [99 99])))

(comment

  (count matrix-p1)

  (count (first matrix-p1))

  (count matrix-ex-p1)

  (count (first matrix-ex-p1))

  (count matrix-p2)

  (count (first matrix-p2))

  (count matrix-ex-p2)

  (count (first matrix-ex-p2))

  (neighbors [0 0])

  (neighbors [5 3])

  (coord-seq matrix)

  (get-in matrix [2 0])

  (time (traverse matrix-ex-p1 [0 0] [9 9]))
  (time (traverse matrix-ex-p2 [0 0] [49 49]))

  (time (traverse matrix-p1 [0 0] [99 99]))

  (time (traverse matrix-p2 [0 0] [499 499]))
  (time (traverse matrix-joseph-p2 [0 0] [499 499]))

  (map first (take 3 (matrix-seq)))

  (def pm (priority-map [0 0] 100 [1 2] 10 [1 3] 1))
  (pm [1 2])

  (pop pm)






  nil)
