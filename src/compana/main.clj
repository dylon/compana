(ns compana.main
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=])
  (:use [compana arithmetic complex])
  (:import [compana.complex Complex Polar]))

(defn -main
  ([& args]
   (let [z (Complex. 1 1)
         w (Complex. 1 1)]
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
     (let [z (Complex. (/ Math/PI 2) 1)]
       (println "   cos(pi/2 + i)  =" (cos z))
       (println "Im(cos(pi/2 + i)) = (e^(-1) - e) / 2 ="
                (/ (- (Math/exp -1) Math/E) 2))
       (println "   sin(pi/2 + i)  =" (sin z))
       (println "Re(sin(pi/2 + i)) = (e^(-1) + e) / 2 ="
                (/ (+ (Math/exp -1) Math/E) 2))
       (println "|cos(pi/2+i) + sin(pi/2+i)| =" (modulus (+ (cos z) (sin z)))))
     (println "Arg -1 =" (Arg (Complex. -1 0)))
     (println "pi =" Math/PI)
     (println "arg -1 =" (arg (Complex. -1 0)))
     (println "pi - pi =" (- Math/PI Math/PI))
     (let [z (Complex. (/ Math/PI 2) Math/PI)]
       (println "cos(pi/2 + i pi) =" (cos z))
       (println "sin(pi/2 + i pi) =" (sin z))
       (println "|(z/|z|)| =" (modulus (/ z (modulus z))))
       (println "theta = Arg z =" (Arg z))))
   (doseq [[z n] [[(Complex. 0 4) 2] [(Complex. 0 8) 3] [(Complex. 1 0) 8]]]
     (printf "(%s + i %s)^(1/%d) = %s%n" (print-str (Re z)) (print-str (Im z)) n
             (->> (nth-roots z n)
               (map print-str)
               (clojure.string/join ", ")
               (format "[%s]"))))
   (println "8th roots of unity to the power 8 = "
            (->> (nth-roots (Complex. 1 0) 8)
              (map (fn [nth-root] (pow nth-root (Complex. 8 0))))
              (map print-str)
              (clojure.string/join ", ")
              (format "[%s]")))
   (println "(2 + i 0)^(3 + i 0) =" (pow (Complex. 2 0) (Complex. 3 0)))
   (println "2/3/4/5 = ((2/3)/4)/5 =" (/ 2 3 4 5))
   (println (Polar. 3 Math/PI))
   ))
