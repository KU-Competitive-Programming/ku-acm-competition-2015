(ns gary.core
  (:gen-class))

(defn OR [x y]
  (or x y))

(defn AND [x y]
  (and x y))

(defn XOR [x y]
  (or (and x (not y)) 
      (and (not x) y)))

(defn polish [bools ops]
  (cond
    (nil? ops) 
      (first bools)
    (and (= 1 (count ops)) (= 2 (count bools)))
      (list (first ops) (first bools) (last bools))
    :else 
      (list 
        (list (first ops) (first bools) (polish (rest bools) (rest ops)))
        (polish (cons (list (first ops) (first bools) (first (rest bools))) (rest (rest bools)))
                (rest ops)))))

(defn leafify [tree]
  (cond 
    (= 2 (count tree))
      (map leafify tree)
    (and (list? (last tree)) (= 2 (count (last tree))))
      (map leafify (list (list (first tree) (second tree) (first (last tree)))
                     (list (first tree) (second tree) (second (last tree)))))
    :else tree))

(defn flat [tree exp]
  (if (seq tree)
    (if (= 2 (count tree))
      (concat (flat (first tree) []) (flat (second tree) []))
      (conj exp tree))
    exp))

(defn count-true [bools ops]
  (count (filter #{true} (map eval (flat (leafify (polish bools ops)) [])))))

(defn -main [& args]
  (use 'gary.core)
  (read-line)
  (doseq [raw (line-seq (java.io.BufferedReader. *in*))]
    (let [line (clojure.string/split raw #" ")
          bools (into [] (for [bool (clojure.string/split (second line) #"")]
                  (if (= bool "T")
                    true
                    false)))
          ops   (into [] (for [op (clojure.string/split (last line) #"")]
                  (cond (= op "|") 'OR
                        (= op "&") 'AND
                        (= op "^") 'XOR)))]
      (println (count-true bools ops)))))
