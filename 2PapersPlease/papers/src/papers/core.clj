(ns papers.core
  (:gen-class))

(def banned #{"BYTELANDIA" "FLATLAND" "LEROY" "JENKINS"})

(defn admitted? [[_ _ v o]]
  (and
    (not= 25 (reduce + (map read-string (clojure.string/split v #""))))
    (not (contains? banned o))))

(defn -main [& args]
  (read-line)
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (let [passport (clojure.string/split line #" ")]
      (if (admitted? passport)
        (println (str "CAUSE NO TROUBLE " (second passport) " " (first passport)))
        (println "ENTRY DENIED")))))
