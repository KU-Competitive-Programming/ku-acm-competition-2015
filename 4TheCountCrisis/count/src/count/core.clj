(ns count.core
  (:gen-class))

(defn ommitted [nums]
  (let [del (Math/abs (/ (- (first nums) (last nums)) (count nums)))
        s (set nums)]
    (loop [v (apply min nums)]
      (if (contains? s v)
        (recur (+ v del))
        v))))

(defn -main [& args]
  (read-line)
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (ommitted (rest (vec (map read-string (clojure.string/split line #" "))))))))
