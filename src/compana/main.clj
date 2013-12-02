(ns compana.main
  (:refer-clojure :exclude [zero? + - * /])
  (:use compana.complex))

(defn -main
  ([& args]
   (let [z (->Complex 1 1)
         w (->Complex 1 1)]
     (println "z + w =" (+ z w))
     (println "z - w =" (- z w))
     (println "z * w =" (* z w))
     (println "z / w =" (/ z w))
     (println "z is 0 =" (zero? z))
     (println "|z| =" (modulus z))
     (println "Arg z =" (Arg z))
     (println "arg z =" (arg z))
     (println "ln z =" (ln z))
     (println "e^z =" (exp z))
     (println "e * cos(1) =" (* Math/E (Math/cos 1)))
     (println "e * sin(1) =" (* Math/E (Math/sin 1)))
     (println "ln e^z =" (ln (exp z)))
     (println "e^(ln z) =" (exp (ln z)))
     (let [z (->Complex (/ Math/PI 2) 1)]
       (println "   cos(pi/2 + i)  =" (cos z))
       (println "Im(cos(pi/2 + i)) = (e^(-1) - e) / 2 =" (/ (- (Math/exp -1) Math/E) 2))
       (println "   sin(pi/2 + i)  =" (sin z))
       (println "Re(sin(pi/2 + i)) = (e^(-1) + e) / 2 =" (/ (+ (Math/exp -1) Math/E) 2))
       (println "|cos(pi/2+i) + sin(pi/2+i)| =" (modulus (+ (cos z) (sin z)))))
     (println "Arg -1 =" (Arg (->Real -1)))
     (println "pi =" Math/PI)
     (println "arg -1 =" (arg (->Real -1)))
     (println "pi - pi =" (- Math/PI Math/PI))
     (let [z (->Complex (/ Math/PI 2) Math/PI)]
       (println "cos(pi/2 + i pi) =" (cos z))
       (println "sin(pi/2 + i pi) =" (sin z))
       (println "|(z/|z|)| =" (modulus (/ z (modulus z))))
       (println "theta = Arg z =" (Arg z))))))
