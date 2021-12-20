(ns core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

;; input is valid clojure!
(def snail-nums (->> (line-seq (io/reader "./input.txt"))
                     (map read-string)))

(defn path-seq
  ([snail-num] (path-seq snail-num [] []))
  ([snail-num prefix acc]
   ;; (println "snail" snail-num)
   ;; (println "p" prefix)
   ;; (println "acc" acc)
   (if (vector? snail-num)
     (let [new-0-path (conj prefix 0)
           new-1-path (conj prefix 1)
           acc'       (conj acc new-0-path new-1-path)]
       (path-seq (second snail-num) new-1-path
         (path-seq (first snail-num) new-0-path acc')))
     acc)))

(comment

  (tree-seq #(and (vector? %) (< 1 (count %)))  [[[[[9,8],1],2],3],4])

  (path-seq [4 [2,3]])

  (path-seq [[2 [2,3]] [1, 2]])

  (def ex1 [[[[[9,8],1],2],3],4])

  (map identity (path-seq ex1))

  (map #(get-in ex1 %) (path-seq ex1))

  (tree [[[[[9,8],1],2],3],4])

  (clojure.walk/postwalk identity [[[[[9,8],1],2],3],4])




  nil)
