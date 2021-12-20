(ns core
  (:require [clojure.java.io :as io]))

(def enhancement-algo-ex (into [] (first (line-seq (io/reader "./simple_input.txt")))))
(def enhancement-algo (into [] (first (line-seq (io/reader "./input.txt")))))
(def image-seed-ex (mapv #(into [] %) (drop 2 (line-seq (io/reader "./simple_input.txt")))))
(def image-seed (mapv #(into [] %) (drop 2 (line-seq (io/reader "./input.txt")))))

(defn output-pixel [image [x y]]
  (let [bits (for [x [(dec x) x (inc x)]
                   y [(dec y) y (inc y)]]
                [x y])]
   (->> bits
        (map #(get-in image (reverse %) \.))
        (map {\. 0 \# 1})
        (apply str)
        ((fn [bin-str] (Long/parseLong bin-str 2))))))

(defn coords [image]
  (let [size (count image)]
    (for [x (range size)
          y (range size)]
     [x y])))

(defn extended-coords [image]
  (let [size (count image)]
    (for [x (range (- 2) (+ size 2))
          y (range (- 2) (+ size 2))]
     [x y])))

(defn transform [image enhancement]
  (let [new-size (+ 4 (count image))]
    (->> (extended-coords image)
      (map #(vector % (get enhancement (output-pixel image %))))
      (into {})
      (reduce (fn [acc [k v]] (assoc-in acc k v)) (vec (repeat new-size (vec (repeat new-size \.))))))))

(defn print-image [image]
  (println "")
  (->> (map (fn [row] (str (apply str row) "\n")) image)
       println)
  (println ""))

(comment
  (+ 1 2)

  (output-pixel image-seed-ex [2 1])

  (get enhancement-algo-ex 34)

  (get-in image-seed-ex (reverse [2 3]))

  (extended-coords image-seed-ex)

  (into {} (map #(vector % (get-in image-seed-ex (reverse %))) (coords image-seed-ex)))

  (print-image (transform image-seed-ex enhancement-algo-ex))


  (vec (repeat 4 (vec (repeat 4 1))))

  (assoc-in [[2 4 6]] [0 1] 10)


  nil)
