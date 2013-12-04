(ns compana.complex
  (:refer-clojure :exclude [zero? + - * /])
  (:use [compana arithmetic real]))

(declare Re Im modulus Arg arg real imaginary complex polar)

(deftype Complex [real imaginary])

(deftype Polar [radius angle])

(defn ->Real [x]
  (->Complex x 0))

(defn ->Imaginary [y]
  (->Complex 0 y))

(def ^:const I (->Imaginary 1))

(defmethod classify Complex [_] :complex)
(defmethod classify Polar [_] :polar)

(defmulti complex classify)

(defmethod complex :complex [^Complex z]
  z)

(defmethod complex :polar [^Polar e]
  (->Complex (* (.radius e) (cos (.angle e)))
             (* (.radius e) (sin (.angle e)))))

(defmethod complex :real [^Number x]
  (->Complex x 0))

(defmulti polar classify)

(defmethod polar :complex [^Complex z]
  (->Polar (modulus z) (Arg z)))

(defmethod polar :polar [^Polar e]
  e)

(defmethod polar :real [^Number x]
  (->Polar (modulus x) 0))

(defmulti Re
  "Real part of a complex number."
  classify)

(defmethod Re :complex [^Complex z]
  (.real z))

(defmethod Re :polar [^Polar e]
  (* (.radius e) (cos (.angle e))))

(defmethod Re :real [^Number x]
  x)

(defmulti Im
  "Imaginary part of a complex number."
  classify)

(defmethod Im :complex [^Complex z]
  (.imaginary z))

(defmethod Im :polar [^Polar e]
  (* (.radius e) (sin (.angle e))))

(defmethod Im :real [^Number x]
  0)

(defmulti modulus
  "Analogous to the absolute value on the real line, returns the length of z on the complex plane."
  classify)

(defmethod modulus :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defmethod modulus :polar [^Polar e]
  (.radius e))

; Equivalent to the absolute value for a real number
(defmethod modulus :real [^Number x]
  (if (< x 0) (- x) x))

(defmulti Arg
  "Argument of complex number, z. This is the angle z forms with the positive, real axis."
  classify)

(defmethod Arg :complex [^Complex z]
  (if (zero? z)
    (throw (IllegalArgumentException.
      "The argument is undefined for zero."))
    (Math/acos (/ (Re z) (modulus z)))))

(defmethod Arg :polar [^Polar e]
  (.angle e))

(defmethod Arg :real [^Number x]
  0)

(defmulti arg
  "Principal argument of z: Arg z, shifted down by pi to bound it between -pi and +pi"
  classify)

(defmethod arg :complex [^Complex z]
  (- (Arg z) Math/PI))

(defmethod arg :polar [^Polar e]
  (- (Arg e) Math/PI))

(defmethod arg :real [^Number x]
  0)

(defmethod nth-roots :complex [^Complex z ^long n]
  (map complex (nth-roots (polar z) n)))

(defmethod nth-roots :polar [^Polar e ^long n]
  (if (zero? e) [0]
    (let [nth-radius (principal-nth-root (.radius e) n)]
      (map #(->Polar nth-radius (+ (/ (.angle e) n) (/ (* 2 % Math/PI) n)))
           (range n)))))

(defmethod + [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ c x) y)))

(defmethod + [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ x c) y)))

(defmethod + [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/+ x u)
               (clojure.core/+ y v))))

(defmethod - [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- c x) y)))

(defmethod - [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- x c) y)))

(defmethod - [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- x u) (clojure.core/- y v))))

(defmethod - [:complex :negation] [^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (- x) (- y))))

(defmethod * [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* c x) (clojure.core/* c y))))

(defmethod * [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* x c) (clojure.core/* y c))))

(defmethod * [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- (clojure.core/* x u) (clojure.core/* y v))
               (clojure.core/+ (clojure.core/* x v) (clojure.core/* y u)))))

(defmethod / [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)
        m (modulus z)
        m*m (clojure.core/* m m)]
    (->Complex (clojure.core// (clojure.core/* c x) m*m)
               (clojure.core// (clojure.core/* c y) m*m))))

(defmethod / [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core// x c)
               (clojure.core// y c))))

(defmethod / [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)
        m (modulus w)
        m*m (clojure.core/* m m)]
    (->Complex
      (clojure.core//
        (clojure.core/+ (clojure.core/* x u)
                        (clojure.core/* y v))
        m*m)
      (clojure.core//
        (clojure.core/- (clojure.core/* y u)
                        (clojure.core/* x v))
        m*m))))

(defmethod zero? Complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (and (zero? x) (zero? y))))

(defmethod zero? Polar [^Polar e]
  (= 0 (.radius e)))

(defmethod ln :complex [^Complex z]
  (->Complex (Math/log (modulus z)) (Arg z)))

(defmethod exp :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (* (Math/exp x) (Math/cos y))
               (* (Math/exp x) (Math/sin y)))))

(defmethod pow [:complex :complex] [^Complex z ^Complex w]
  (exp (* w (ln z))))

(defmethod cos :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (+ (exp I*z) (exp (- I*z)))
       2)))

(defmethod sin :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (- (exp I*z) (exp (- I*z)))
       (* 2 I))))

(defmulti real classify)

(defmethod real :complex [^Complex z] (Re z))

(defmethod real :real [^Number x] x)

(defmulti imaginary classify)

(defmethod imaginary :complex [^Complex z] (Im z))

(defmethod imaginary :real [^Number x] 0)

(defmethod print-method Complex [^Complex z ^java.io.Writer w]
  (.write w "Complex{")
  (.write w (print-str (if (zero? (Re z)) 0 (Re z))))
  (.write w " + i * ")
  (.write w (print-str (if (zero? (Im z)) 0 (Im z))))
  (.write w "}"))

(defmethod print-method Polar [^Polar e ^java.io.Writer w]
  (.write w "Polar{")
  (.write w (print-str (if (zero? (.radius e)) 0 (.radius e))))
  (.write w " * e^(i * ")
  (.write w (print-str (if (zero? (.angle e)) 0 (.angle e))))
  (.write w ")}"))

