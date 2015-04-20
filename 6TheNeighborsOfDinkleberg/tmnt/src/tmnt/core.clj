; TEENAGE MUTANT NINJA TURTLES DAR NAR NAR NAR NAR NAR NAAARRRRR
(ns tmnt.core
  (:gen-class))

(defn tmnns
  [[c _ & cs]]
  (let [refs (concat (partition 2 cs) (partition 2 (reverse cs)))]
    (for [n (range 1 (+ c 1))]
      (reduce + (for [ngs (filter (fn [c] (= (first c) n)) refs)]
                  (count (filter (fn [c] (= (first c) (second ngs))) refs)))))))

(defn -main [& args]
  (read-line)
  (doseq [raw (line-seq (java.io.BufferedReader. *in*))]
    (let [line (map read-string (clojure.string/split raw #" "))]
    (printl (clojure.string/join " " (tmnns line))))))
