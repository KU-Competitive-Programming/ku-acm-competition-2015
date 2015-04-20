(ns cypher.core
  (:gen-class))

; 0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
; a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
(defn encode [s]
  (apply str (map #(if (= % \ ) 
                     %
                     (char (+ (mod (+ (- (int %) 65) 13) 26) 65)))
                  s)))

(defn -main [& args]
  (doseq [line (line-seq (java.io.BufferedReader. *in*))]
    (println (encode line))))
