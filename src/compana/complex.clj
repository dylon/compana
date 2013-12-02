(ns compana.complex
  (:refer-clojure :exclude [zero? + - * /]))

(declare zero? + - * / ln exp sin cos real imaginary ->Real ->Imaginary)

(defprotocol IComplex
  (Re [z]
    "Real part of this complex number.")
  (Im [z]
    "Imaginary part of this complex number.")
  (modulus [z]
    "Analogous to the absolute value on the real line, returns the length of z on the complex plane.")
  (Arg [z]
    "Argument of complex number, z. This is the angle z forms with the positive, real axis.")
  (arg [z]
    "Principal argument of z: Arg z, shifted down by pi to bound it between -pi and +pi"))

(deftype Complex [real imaginary]
  IComplex
  (Re [z] real)
  (Im [z] imaginary)
  (modulus [z]
    (let [x (Re z) y (Im z)]
      (Math/sqrt (+ (* x x) (* y y)))))
  (Arg [z]
    (if (zero? z)
      (throw (IllegalArgumentException.
        "The argument is undefined for zero."))
      (Math/acos (/ (Re z) (modulus z)))))
  (arg [z]
    (- (Arg z) Math/PI)))

(defn ->Real [x]
  (->Complex x 0))

(defn ->Imaginary [y]
  (->Complex 0 y))

(def ^:const I (->Imaginary 1))

(defn classify [x]
  (if (instance? Complex x)
    :complex
    :real))

(defmulti +
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod + [:real :real] [x y]
  (clojure.core/+ x y))

(defmethod + [:real :complex] [c z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ c x) y)))

(defmethod + [:complex :real] [z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ x c) y)))

(defmethod + [:complex :complex] [z w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/+ x u)
               (clojure.core/+ y v))))

(defmethod + :variadic [x y & others]
  (apply + (+ x y) others))

(defmulti -
  (fn
    ([x] [(classify x) :negation])
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod - [:real :real] [x y]
  (clojure.core/- x y))

(defmethod - [:real :complex] [c z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- c x) y)))

(defmethod - [:complex :real] [z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- x c) y)))

(defmethod - [:complex :complex] [z w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- x u) (clojure.core/- y v))))

(defmethod - [:real :negation] [x]
  (clojure.core/- x))

(defmethod - [:complex :negation] [z]
  (let [x (Re z) y (Im z)]
    (->Complex (- x) (- y))))

(defmethod - :variadic [x y & others]
  (apply - (- x y) others))

(defmulti *
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & others] :variadic)))

(defmethod * [:real :real] [x y]
  (clojure.core/* x y))

(defmethod * [:real :complex] [c z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* c x) (clojure.core/* c y))))

(defmethod * [:complex :real] [z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* x c) (clojure.core/* y c))))

(defmethod * [:complex :complex] [z w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- (clojure.core/* x u) (clojure.core/* y v))
               (clojure.core/+ (clojure.core/* x v) (clojure.core/* y u)))))

(defmethod * :variadic [x y & others])

(defmulti /
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & others] :variadic)))

(defmethod / [:real :real] [x y]
  (clojure.core// x y))

(defmethod / [:real :complex] [c z]
  (let [x (Re z) y (Im z)
        m (modulus z)
        m2 (clojure.core/* m m)]
    (->Complex (clojure.core// (clojure.core/* c x) m2)
               (clojure.core// (clojure.core/* c y) m2))))

(defmethod / [:complex :real] [z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core// x c)
               (clojure.core// y c))))

(defmethod / [:complex :complex] [z w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)
        m (modulus w)
        m2 (clojure.core/* m m)]
    (->Complex
      (clojure.core//
        (clojure.core/+ (clojure.core/* x u)
                        (clojure.core/* y v))
        m2)
      (clojure.core//
        (clojure.core/- (clojure.core/* y u)
                        (clojure.core/* x v))
        m2))))

(defmethod / :variadic [x y & others]
  (apply - (- x y) others))

(defmulti zero? class)
(defmethod zero? Long [x]
  (= 0 x))
(defmethod zero? Double [x]
  (if (> x 0)
    (< x  1e-15)
    (> x -1e-15)))
(defmethod zero? Complex [z]
  (let [x (Re z) y (Im z)]
    (and (zero? x) (zero? y))))

(defmulti ln classify)
(defmethod ln :real [x]
  (Math/log x))
(defmethod ln :complex [z]
  (->Complex (Math/log (modulus z)) (Arg z)))

(defmulti exp classify)
(defmethod exp :real [x]
  (Math/exp x))
(defmethod exp :complex [z]
  (let [x (Re z) y (Im z)]
    (->Complex (* (Math/exp x) (Math/cos y))
               (* (Math/exp x) (Math/sin y)))))

(defmulti cos classify)
(defmethod cos :real [x] (Math/cos x))
(defmethod cos :complex [z]
  (let [Iz (* I z)]
    (/ (+ (exp Iz) (exp (- Iz)))
       2)))

(defmulti sin classify)
(defmethod sin :real [x] (Math/sin x))
(defmethod sin :complex [z]
  (let [Iz (* I z)]
    (/ (- (exp Iz) (exp (- Iz)))
       (* 2 I))))

(defmulti real classify)
(defmethod real :real [x] x)
(defmethod real :complex [z] (Re z))

(defmulti imaginary classify)
(defmethod imaginary :real [x] 0)
(defmethod imaginary :complex [z] (Im z))

(defmethod print-method Complex [^Complex z ^java.io.Writer w]
  (.write w "Complex[")
  (.write w (print-str (Re z)))
  (.write w ", ")
  (.write w (print-str (Im z)))
  (.write w "]"))

