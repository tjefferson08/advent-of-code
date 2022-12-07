(def FILENAME "./input.txt")
;; (def FILENAME "./example.txt")

(defn lines [] (line-seq (io/reader FILENAME)))

(defn next-cwd [cwd change]
  (into [] (case change
             "/" ["/"]
             ".." (butlast cwd)
             (conj cwd change))))

(defn parse [cwd fs [line & remaining]]
  (let [[seg1 seg2 seg3] line]
    (cond
      (nil? line) fs
      (= ["$" "cd"] [seg1 seg2]) (recur (next-cwd cwd seg3) fs remaining)
      (= ["$" "ls"] [seg1 seg2]) (recur cwd fs remaining)
      (= "dir" seg1) (recur cwd (update-in fs cwd assoc seg2 {}) remaining)
      :else (recur cwd (update-in fs cwd assoc seg2 (Integer/parseInt seg1)) remaining))))

(defn size [fs]
 (if (number? fs)
   fs
   (reduce (fn [acc fs'] (+ acc (size fs'))) 0 (vals fs))))

(defn dirs [fs]
 (if (number? fs)
     []
     (let [top-level-dirs (filter map? (vals fs))]
       (concat top-level-dirs (mapcat dirs top-level-dirs)))))


(comment

  (next-cwd ["/" "a" "b"] "..")
  (next-cwd ["/" "a" "b"] "/")
  (next-cwd ["/" "a" "b"] "c")

  (size 123)
  (size {"/" {"a" 123}})
  (dirs {"/" {"a" 123}})

  (size {"c" 123 "b" 123 "d" {"e" 123}})
  (dirs {"c" 123 "b" 123 "f" {"d" {"e" 123}}})

  (update-in {"/" {"a" {"b" 123} "d" 123}} ["/" "a"] assoc "c" {})


  (let [fs (->> (lines)
                (map #(str/split % #" "))
                (parse ["/"], {}))
        du (size fs)
        unused (- 70000000 du)
        free-target (- 30000000 unused)
        dir-coll (dirs fs)
        sizes (sort (map size dir-coll))
        viable-targets (drop-while #(> free-target %) sizes)]
    (first viable-targets))


  nil)
