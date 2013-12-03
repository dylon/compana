(ns compana.complex
  (:refer-clojure :exclude [zero? + - * /]))

(declare
  zero? + - * /
  Re Im modulus Arg arg
  nth-roots principal-nth-root
  ln exp pow sin cos real imaginary
  ->Real ->Imaginary complex polar)

(deftype Complex [real imaginary])

(deftype Polar [radius angle])

(defn ->Real [x]
  (->Complex x 0))

(defn ->Imaginary [y]
  (->Complex 0 y))

(def ^:const I (->Imaginary 1))

(def ^:const ZERO 1e-15)

(defn classify [x]
  (condp instance? x
    Complex :complex
    Polar :polar
    :real))

(defmulti complex classify)

(defmethod complex :complex [^Complex z]
  z)

(defmethod complex :polar [^Polar e]
  (->Complex (* (.radius e) (cos (.angle e)))
             (* (.radius e) (sin (.angle e)))))

(defmethod complex :real [x]
  (->Complex x 0))

(defmulti polar classify)

(defmethod polar :complex [^Complex z]
  (->Polar (modulus z) (Arg z)))

(defmethod polar :polar [^Polar e]
  e)

(defmethod polar :real [x]
  (->Polar (modulus x) 0))

(defmulti Re
  "Real part of a complex number."
  classify)

(defmethod Re :complex [^Complex z]
  (.real z))

(defmethod Re :real [x]
  x)

(defmulti Im
  "Imaginary part of a complex number."
  classify)

(defmethod Im :complex [^Complex z]
  (.imaginary z))

(defmethod Im :real [x]
  0)

(defmulti modulus
  "Analogous to the absolute value on the real line, returns the length of z on the complex plane."
  classify)

(defmethod modulus :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (Math/sqrt (+ (* x x) (* y y)))))

; Equivalent to the absolute value for a real number
(defmethod modulus :real [x]
  (if (< x 0) (- x) x))

(defmulti Arg
  "Argument of complex number, z. This is the angle z forms with the positive, real axis."
  classify)

(defmethod Arg :complex [^Complex z]
  (if (zero? z)
    (throw (IllegalArgumentException.
      "The argument is undefined for zero."))
    (Math/acos (/ (Re z) (modulus z)))))

(defmethod Arg :real [x]
  0)

(defmulti arg
  "Principal argument of z: Arg z, shifted down by pi to bound it between -pi and +pi"
  classify)

(defmethod arg :complex [^Complex z]
  (- (Arg z) Math/PI))

(defmethod arg :real [x]
  0)

(defmulti nth-roots
  "Finds all n roots of the complex number if it is not zero, or [zero] if it is."
  (fn [x ^long n] (classify x)))

(defmethod nth-roots :complex [^Complex z ^long n]
  (map complex (nth-roots (polar z) n)))

(defmethod nth-roots :polar [^Polar e ^long n]
  (if (zero? e) [0]
    (let [nth-radius (principal-nth-root (.radius e) n)]
      (map #(->Polar nth-radius (+ (/ (.angle e) n) (/ (* 2 % Math/PI) n)))
           (range n)))))

(defmulti principal-nth-root
  "Returns the positive, nth root of a positive, real number."
  (fn [x ^long n] {:pre [(>= x 0) (pos? n)]} (classify x)))

;; Finds the principal, nth root of the non-negative, real number x via the
;; Newton method:
;;  1. Make an initial guess r_0
;;  2. Set r_(k+1) = r_k - f(r_k) / f'(r_k)
;;  3. Repeat until the desired precision is reached.
;; Derivation:
;;  1. r = x^(1/n), where r is the nth root of x
;;  2. r = x^(1/n) <=> r^n = x <=> r^n - x = 0
;;  3. Let f(r) = r^n - x = 0
;;  4. Therefore, f'(r) = n * r^(n-1)
;;  5. r_(k+1) = r_k - f(r_k) / f'(r_k)
;;             = r_k - (r_k^n - x) / (n * r_k^(n-1))
;;             = r_k - (r_k / n) + (x / (n * x_k^(n-1)))
;;             = (1 / n) * ((n - 1) * r_k + x / r_k^(n-1))
(defmethod principal-nth-root :real [x ^long n]
  (loop [root 1.0]
    (let [delta (* (/ n x) (- (/ x (Math/pow root (dec n))) root))]
      (if (< (Math/abs delta) ZERO) root
        (recur (+ root delta))))))

(defmulti +
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod + [:real :real] [x y]
  (clojure.core/+ x y))

(defmethod + [:real :complex] [c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ c x) y)))

(defmethod + [:complex :real] [^Complex z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/+ x c) y)))

(defmethod + [:complex :complex] [^Complex z ^Complex w]
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

(defmethod - [:real :complex] [c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- c x) y)))

(defmethod - [:complex :real] [^Complex z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/- x c) y)))

(defmethod - [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- x u) (clojure.core/- y v))))

(defmethod - [:real :negation] [x]
  (clojure.core/- x))

(defmethod - [:complex :negation] [^Complex z]
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

(defmethod * [:real :complex] [c ^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* c x) (clojure.core/* c y))))

(defmethod * [:complex :real] [^Complex z c]
  (let [x (Re z) y (Im z)]
    (->Complex (clojure.core/* x c) (clojure.core/* y c))))

(defmethod * [:complex :complex] [^Complex z w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (->Complex (clojure.core/- (clojure.core/* x u) (clojure.core/* y v))
               (clojure.core/+ (clojure.core/* x v) (clojure.core/* y u)))))

(defmethod * :variadic [x y & others]
  (apply * (* x y) others))

(defmulti /
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & others] :variadic)))

(defmethod / [:real :real] [x y]
  (clojure.core// x y))

(defmethod / [:real :complex] [c ^Complex z]
  (let [x (Re z) y (Im z)
        m (modulus z)
        m*m (clojure.core/* m m)]
    (->Complex (clojure.core// (clojure.core/* c x) m*m)
               (clojure.core// (clojure.core/* c y) m*m))))

(defmethod / [:complex :real] [^Complex z c]
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

(defmethod / :variadic [x y & others]
  (apply - (- x y) others))

(defmulti zero? class)
(defmethod zero? Long [x]
  (= 0 x))
(defmethod zero? clojure.lang.Ratio [x]
  (= 0 (numerator x)))
(defmethod zero? Double [x]
  (< (Math/abs x) ZERO))
(defmethod zero? Complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (and (zero? x) (zero? y))))
(defmethod zero? Polar [^Polar e]
  (= 0 (.radius e)))

(defmulti ln classify)
(defmethod ln :real [x]
  (Math/log x))
(defmethod ln :complex [^Complex z]
  (->Complex (Math/log (modulus z)) (Arg z)))

(defmulti exp classify)
(defmethod exp :real [x]
  (Math/exp x))
(defmethod exp :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (->Complex (* (Math/exp x) (Math/cos y))
               (* (Math/exp x) (Math/sin y)))))

(defmulti pow
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod pow [:real :real] [x y]
  (Math/pow x y))

(defmethod pow [:complex :complex] [z w]
  (exp (* w (ln z))))

(defmethod pow :variadic [x & args]
  (pow x (apply pow args)))

(defmulti cos classify)
(defmethod cos :real [x] (Math/cos x))
(defmethod cos :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (+ (exp I*z) (exp (- I*z)))
       2)))

(defmulti sin classify)
(defmethod sin :real [x] (Math/sin x))
(defmethod sin :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (- (exp I*z) (exp (- I*z)))
       (* 2 I))))

(defmulti real classify)
(defmethod real :real [x] x)
(defmethod real :complex [^Complex z] (Re z))

(defmulti imaginary classify)
(defmethod imaginary :real [x] 0)
(defmethod imaginary :complex [^Complex z] (Im z))

(defmethod print-method Complex [^Complex z ^java.io.Writer w]
  (.write w "Complex[")
  (.write w (print-str (Re z)))
  (.write w ", ")
  (.write w (print-str (Im z)))
  (.write w "]"))

