(ns harmonious.core
  (:gen-class))

(defn prime? [n]
  (if (even? n) false
    (let [root (num (int (Math/sqrt n)))]
      (loop [i 3]
        (if (> i root) true
          (if (zero? (mod n i)) false
            (recur (+ i 2))))))))

(defn harmonious? [p]
  (and (prime? (count p)) (every? true? (for [row p]
                                         (prime? (count row))))))

(defn -main [& args]
  (let [poly (filter string? (for [line (line-seq (java.io.BufferedReader. *in*))]
                                    (if (not= line "0")
                                      line
                                      nil)))]
    (if (harmonious? poly)
      (println "Harmonious")
      (println "Cacophonous"))))
