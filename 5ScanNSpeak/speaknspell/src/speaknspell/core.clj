(ns speaknspell.core
  (:gen-class))

(defn speak [s]
  (apply str
         (flatten (map #(vector (count %) (first %)) (partition-by identity s)))))

(defn -main [& args]
  (read-line)
  (doseq [raw (line-seq (java.io.BufferedReader. *in*))]
    (let [line (map read-string (clojure.string/split raw #""))]
      (println (speak line)))))
