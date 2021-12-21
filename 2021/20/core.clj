(ns core
  (:require [clojure.java.io :as io]))

(def enhancement-algo-ex (into [] (first (line-seq (io/reader "./simple_input.txt")))))
(def enhancement-algo (into [] (first (line-seq (io/reader "./input.txt")))))
(def image-seed-ex (mapv #(into [] %) (drop 2 (line-seq (io/reader "./simple_input.txt")))))
(def image-seed-mp-ex (->> (line-seq (io/reader "./simple_input.txt"))
                           (drop 2)
                           (map-indexed (fn [y row] (map-indexed (fn [x el] [[x y] el]) row)))
                           (apply concat)
                           (into {})))
(def image-seed-mp (->> (line-seq (io/reader "./input.txt"))
                        (drop 2)
                        (map-indexed (fn [y row] (map-indexed (fn [x el] [[x y] el]) row)))
                        (apply concat)
                        (into {})))

(def image-seed (mapv #(into [] %) (drop 2 (line-seq (io/reader "./input.txt")))))

(defn output-pixel [image [x y]]
  (let [bits (for [y [(dec y) y (inc y)]
                   x [(dec x) x (inc x)]]
               [x y])]
    (->> bits
         (map #(get image % \.))
         (map {\. 0 \# 1})
         (apply str))))
         ;; ((fn [bin-str] (Long/parseLong bin-str 2))))))


(defn output-coords [[x y :as p]]
  (for [x (range (- x 2) (+ x 3))
        y (range (- y 2) (+ y 3))]
    [x y]))

(defn transform [image enhancement]
  (->> (keys image)
       (mapcat output-coords)
       (map #(vector % (get enhancement (output-pixel image %))))
       (into {})))


(comment
  (+ 1 2)

  (count (output-coords [2 2]))

  (->> [[0 2]]
       (mapcat output-coords)
       (map #(vector % (output-pixel image-seed-mp-ex %))))

  (->> [[99 99]]
       (mapcat output-coords)
       (map #(vector % (output-pixel image-seed-mp %))))

  (get enhancement-algo-ex (output-pixel image-seed-mp-ex [2 2]))
  (get enhancement-algo-ex (output-pixel image-seed-mp-ex [0 3]))
  (output-pixel image-seed-mp-ex [0 3])

  (get enhancement-algo-ex 192)
  (get enhancement-algo 8)

  (sort image-seed-mp)

  (as-> image-seed-mp-ex _
    (transform _ enhancement-algo-ex)
    (transform _ enhancement-algo-ex)
    (sort _))
    ;; (filter (fn [[k v]] (= v \#)) _)
    ;; (count _))

  (as-> image-seed-mp _
    (transform _ enhancement-algo)
    (transform _ enhancement-algo)
    (sort _))
    ;; (filter (fn [[k v]] (= v \#)) _))
    ;; (count _))

    ;; (transform _ enhancement-algo-ex)
    ;; (filter (fn [[k v]] (= v \#)) _)
    ;; (count _))


  nil)
