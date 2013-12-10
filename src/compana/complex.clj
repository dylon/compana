(ns compana.complex
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=])
  (:use [compana arithmetic real])
  (:import [compana.complex Complex Polar]))

(declare Re Im modulus arg Arg complex polar)

(def I (Complex. 0 1))

;; --------------------------------------
;; classify
;; --------------------------------------

(defmethod classify Complex [_] :complex)
(defmethod classify Polar [_] :polar)

;; --------------------------------------
;; complex
;; --------------------------------------

(defmulti complex classify)

(defmethod complex :complex [^Complex z]
  z)

(defmethod complex :polar [^Polar e]
  (Complex. (* (.radius e) (cos (.angle e)))
            (* (.radius e) (sin (.angle e)))))

(defmethod complex :real [^Number x]
  (Complex. x 0))

(defmethod complex '(:real :real) [^Number x ^Number y]
  (Complex. x y))

;; --------------------------------------
;; polar
;; --------------------------------------

(defmulti polar classify)

(defmethod polar :complex [^Complex z]
  (Polar. (modulus z) (arg z)))

(defmethod polar :polar [^Polar e]
  e)

(defmethod polar :real [^Number x]
  (Polar. (modulus x) 0))

(defmethod polar '(:real :real) [^Number radius ^Number angle]
  (Polar. radius angle))

;; --------------------------------------
;; Re
;; --------------------------------------

(defmulti Re
  "Real part of a complex number."
  classify)

(defmethod Re :complex [^Complex z]
  (.real z))

(defmethod Re :polar [^Polar e]
  (* (.radius e) (cos (.angle e))))

(defmethod Re :real [^Number x]
  x)

;; --------------------------------------
;; Im
;; --------------------------------------

(defmulti Im
  "Imaginary part of a complex number."
  classify)

(defmethod Im :complex [^Complex z]
  (.imaginary z))

(defmethod Im :polar [^Polar e]
  (* (.radius e) (sin (.angle e))))

(defmethod Im :real [^Number x]
  0)

;; --------------------------------------
;; conjugate
;; --------------------------------------

(defmulti conjugate classify)

(defmethod conjugate :real [^Number x]
  x)

(defmethod conjugate :complex [^Complex z]
  (Complex. (Re z) (- (Im z))))

(defmethod conjugate :polar [^Polar z]
  (Polar. (.radius z) (mod (- (.angle z)) (* 2 Math/PI))))

;; --------------------------------------
;; modulus
;; --------------------------------------

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
  (abs x))

(defmethod abs :complex [^Complex z]
  (modulus z))

(defmethod abs :polar [^Polar z]
  (modulus z))

;; --------------------------------------
;; arg
;; --------------------------------------

(defmulti arg
  "Argument of complex number, z. This is the angle z forms with the positive, real axis."
  classify)

(defmethod arg :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (Math/atan2 y x)))

(defmethod arg :polar [^Polar e]
  (.angle e))

(defmethod arg :real [^Number x]
  0)

;; --------------------------------------
;; Arg
;; --------------------------------------

(defmulti Arg
  "Principal argument of z: arg z, shifted down by pi to bound it by [-pi, +pi)"
  classify)

(defmethod Arg :complex [^Complex z]
  (- (arg z) Math/PI))

(defmethod Arg :polar [^Polar e]
  (- (arg e) Math/PI))

(defmethod Arg :real [^Number x]
  0)

;; --------------------------------------
;; nth-roots
;; --------------------------------------

(defmethod nth-roots :complex [^Complex z ^long n]
  (map complex (nth-roots (polar z) n)))

(defmethod nth-roots :polar [^Polar e ^long n]
  (if (zero? e) [0]
    (let [nth-radius (principal-nth-root (.radius e) n)]
      (map #(Polar. nth-radius (+ (/ (.angle e) n) (/ (* 2 % Math/PI) n)))
           (range n)))))

(defmethod principal-sqrt :complex [^Complex z]
  (complex (principal-sqrt (polar z))))

(defmethod principal-sqrt :polar [^Polar z]
  (let [radius (principal-sqrt (.radius z))
        angle (/ (.angle z) 2)]
    (Polar. radius angle)))

;; --------------------------------------
;; Inequalities
;; --------------------------------------
;; Over the complex plane, I'm defining an ordering as follows:
;; 1. z < w
;;   a. Re(z) < Re(w)
;;   b. Re(z) = Re(w) and Im(z) < Im(w)
;; 2. z = w
;;   a. Re(z) = Re(w) and Im(z) = Im(w)
;; 3. z > w
;;   a. Re(z) > Re(w)
;;   b. Re(z) = Re(w) and Im(z) > Im(w)
;;
;; Note that this ordering does not yield an ordered field.

;; --------------------------------------
;; <
;; --------------------------------------

(defmethod < [:complex :real] [^Complex z, ^Number c]
  (let [x (Re z) y (Im z)]
    (or
      (< x c)
      (and (== x c) (< y 0)))))

(defmethod < [:real :complex] [^Number c, ^Complex z]
  (let [x (Re z) y (Im z)]
    (or
      (< c x)
      (and (== c x) (< 0 y)))))

(defmethod < [:complex :complex] [^Complex z, ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (or
      (< x u)
      (and (== x u) (< y v)))))

(defmethod < [:polar :real] [^Polar z, ^Number c]
  (< (complex z) c))

(defmethod < [:real :polar] [^Number c, ^Polar z]
  (< c (complex z)))

(defmethod < [:polar :complex] [^Polar z, ^Complex w]
  (< (complex z) w))

(defmethod < [:complex :polar] [^Complex z, ^Polar w]
  (< z (complex w)))

(defmethod < [:polar :polar] [^Polar z, ^Polar w]
  (< (complex z) (complex w)))

;; --------------------------------------
;; =
;; --------------------------------------

(defmethod = [:complex :real] [^Complex z, ^Number c]
  (let [x (Re z) y (Im z)]
    (and (= y 0) (= x c))))

(defmethod = [:real :complex] [^Number c, ^Complex z]
  (= z c))

(defmethod = [:complex :complex] [^Complex z, ^Complex w]
  (let [x (Re z), y (Im z)
        u (Re w), v (Im w)]
    (and (= x u) (= y v))))

(defmethod = [:polar :real] [^Polar z, ^Number c]
  (= (complex z) c))

(defmethod = [:real :polar] [^Number c, ^Polar z]
  (= c (complex z)))

(defmethod = [:polar :complex] [^Polar z, ^Complex w]
  (= (complex z) w))

(defmethod = [:complex :polar] [^Complex z, ^Polar w]
  (= z (complex w)))

(defmethod = [:polar :polar] [^Polar z, ^Polar w]
  (= (complex z) (complex w)))

;; --------------------------------------
;; ==
;; --------------------------------------
;; Single "=" is general equality, where double "==" is defined only on numbers.

(defmethod == [:complex :real] [^Complex z, ^Number c]
  (let [x (Re z) y (Im z)]
    (and (== y 0) (== x c))))

(defmethod == [:real :complex] [^Number c, ^Complex z]
  (== z c))

(defmethod == [:complex :complex] [^Complex z, ^Complex w]
  (let [x (Re z), y (Im z)
        u (Re w), v (Im w)]
    (and (== x u) (== y v))))

(defmethod == [:polar :real] [^Polar z, ^Number c]
  (== (complex z) c))

(defmethod == [:real :polar] [^Number c, ^Polar z]
  (== c (complex z)))

(defmethod == [:polar :complex] [^Polar z, ^Complex w]
  (== (complex z) w))

(defmethod == [:complex :polar] [^Complex z, ^Polar w]
  (== z (complex w)))

(defmethod == [:polar :polar] [^Polar z, ^Polar w]
  (== (complex z) (complex w)))

;; --------------------------------------
;; >
;; --------------------------------------

(defmethod > [:complex :real] [^Complex z, ^Number c]
  (let [x (Re z), y (Im z)]
    (or
      (> x c)
      (and (== x c) (> y 0)))))

(defmethod > [:real :complex] [^Number c, ^Complex z]
  (let [x (Re z), y (Im z)]
    (or
      (> c x)
      (and (== c x) (> 0 y)))))

(defmethod > [:complex :complex] [^Complex z, ^Complex w]
  (let [x (Re z), y (Im z)
        u (Re w), v (Im w)]
    (or
      (> x u)
      (and (== x u) (> y v)))))

(defmethod > [:polar :real] [^Polar z, ^Number c]
  (> (complex z) c))

(defmethod > [:real :polar] [^Number c, ^Polar z]
  (> c (complex z)))

(defmethod > [:polar :complex] [^Polar z, ^Complex w]
  (> (complex z) w))

(defmethod > [:complex :polar] [^Complex z, ^Polar w]
  (> z (complex w)))

(defmethod > [:polar :polar] [^Polar z, ^Polar w]
  (> (complex z) (complex w)))

;; --------------------------------------
;; <=
;; --------------------------------------

(defmethod <= [:complex :real] [^Complex z, ^Number c]
  (or (< z c) (== z c)))

(defmethod <= [:real :complex] [^Number c, ^Complex z]
  (or (< c z) (== c z)))

(defmethod <= [:complex :complex] [^Complex z, ^Complex w]
  (or (< z w) (== z w)))

(defmethod <= [:polar :real] [^Polar z, ^Number c]
  (or (< z c) (== z c)))

(defmethod <= [:real :polar] [^Number c, ^Polar z]
  (or (< c z) (== c z)))

(defmethod <= [:polar :complex] [^Polar z, ^Complex w]
  (or (< z w) (== z w)))

(defmethod <= [:complex :polar] [^Complex z, ^Polar w]
  (or (< z w) (== z w)))

(defmethod <= [:polar :polar] [^Polar z, ^Polar w]
  (or (< z w) (== z w)))

;; --------------------------------------
;; >=
;; --------------------------------------

(defmethod >= [:complex :real] [^Complex z, ^Number c]
  (or (> z c) (== z c)))

(defmethod >= [:real :complex] [^Number c, ^Complex z]
  (or (> c z) (== c z)))

(defmethod >= [:complex :complex] [^Complex z, ^Complex w]
  (or (> z w) (== z w)))

(defmethod >= [:polar :real] [^Polar z, ^Number c]
  (or (> z c) (== z c)))

(defmethod >= [:real :polar] [^Number c, ^Polar z]
  (or (> c z) (== c z)))

(defmethod >= [:polar :complex] [^Polar z, ^Complex w]
  (or (> z w) (== z w)))

(defmethod >= [:complex :polar] [^Complex z, ^Polar w]
  (or (> z w) (== z w)))

(defmethod >= [:polar :polar] [^Polar z, ^Polar w]
  (or (> z w) (== z w)))

;; --------------------------------------
;; +
;; --------------------------------------

(defmethod + [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/+ c x) y)))

(defmethod + [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/+ x c) y)))

(defmethod + [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (Complex. (clojure.core/+ x u)
              (clojure.core/+ y v))))

(defmethod + [:real :polar] [^Number c ^Polar z]
  (->> (complex z) (+ c) polar))

(defmethod + [:polar :real] [^Polar z ^Number c]
  (-> (complex z) (+ c) polar))

(defmethod + [:polar :polar] [^Polar z ^Polar w]
  (polar (+ (complex z) (complex w))))

(defmethod + [:polar :complex] [^Polar z ^Complex w]
  (polar (+ (complex z) w)))

(defmethod + [:complex :polar] [^Complex z ^Polar w]
  (+ z (complex w)))

;; --------------------------------------
;; -
;; --------------------------------------

(defmethod - [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/- c x) y)))

(defmethod - [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/- x c) y)))

(defmethod - [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (Complex. (clojure.core/- x u) (clojure.core/- y v))))

(defmethod - [:complex :negation] [^Complex z]
  (let [x (Re z) y (Im z)]
    (Complex. (- x) (- y))))

(defmethod - [:real :polar] [^Number c ^Polar z]
  (->> (complex z) (- c) polar))

(defmethod - [:polar :real] [^Polar z ^Number c]
  (-> (complex z) (- c) polar))

(defmethod - [:polar :polar] [^Polar z ^Polar w]
  (polar (- (complex z) (complex w))))

(defmethod - [:polar :complex] [^Polar z ^Complex w]
  (polar (- (complex z) w)))

(defmethod - [:complex :polar] [^Complex z ^Polar w]
  (- z (complex w)))

;; --------------------------------------
;; *
;; --------------------------------------

(defmethod * [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/* c x) (clojure.core/* c y))))

(defmethod * [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core/* x c) (clojure.core/* y c))))

(defmethod * [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)]
    (Complex. (clojure.core/- (clojure.core/* x u) (clojure.core/* y v))
              (clojure.core/+ (clojure.core/* x v) (clojure.core/* y u)))))

(defmethod * [:real :polar] [^Number c, ^Polar z]
  (if (< c 0)
    (Polar.
      (* (modulus c) (.radius z))
      (mod (- (.angle z)) (* 2 Math/PI)))
    (Polar. (* c (.radius z)) (.angle z))))

(defmethod * [:polar :real] [^Polar z, ^Number c]
  (* c z))

(defmethod * [:polar :polar] [^Polar z, ^Polar w]
  (let [radius (* (.radius z) (.radius w))
        angle (mod (+ (.angle z) (.angle w)) (* 2 Math/PI))]
    (Polar. radius angle)))

(defmethod * [:polar :complex] [^Polar z, ^Complex w]
  (* z (polar w)))

(defmethod * [:complex :polar] [^Complex z, ^Polar w]
  (* z (complex w)))

;; --------------------------------------
;; /
;; --------------------------------------

(defmethod / [:real :complex] [^Number c ^Complex z]
  (let [x (Re z) y (Im z)
        m (modulus z)
        m*m (clojure.core/* m m)]
    (Complex. (clojure.core// (clojure.core/* c x) m*m)
              (clojure.core// (clojure.core/* c y) m*m))))

(defmethod / [:complex :real] [^Complex z ^Number c]
  (let [x (Re z) y (Im z)]
    (Complex. (clojure.core// x c)
              (clojure.core// y c))))

(defmethod / [:complex :complex] [^Complex z ^Complex w]
  (let [x (Re z) y (Im z)
        u (Re w) v (Im w)
        m (modulus w)
        m*m (clojure.core/* m m)]
    (Complex.
      (clojure.core//
        (clojure.core/+ (clojure.core/* x u)
                        (clojure.core/* y v))
        m*m)
      (clojure.core//
        (clojure.core/- (clojure.core/* y u)
                        (clojure.core/* x v))
        m*m))))

(defmethod / [:real :polar] [^Number c, ^Polar z]
  (if (< c 0)
    (Polar. (* (modulus c) (.radius z))
            (.angle z))
    (Polar. (* c (.radius z))
            (mod (- (.angle z)) (* 2 Math/PI)))))

(defmethod / [:polar :real] [^Polar z, ^Number c]
  (cond
    (zero? c) (throw (ArithmeticException.
                       (str "Division by zero: cannot divide " z " / " c)))
    (< c 0) (Polar. (/ (.radius z) (modulus c))
                    (mod (- (.angle z)) (* 2 Math/PI)))
    :else (Polar. (/ (.radius z) c)
                  (.angle z))))

(defmethod / [:polar :polar] [^Polar z, ^Polar w]
  (Polar. (/ (.radius z) (.radius w))
          (mod (- (.angle z) (.angle w)) (* 2 Math/PI))))

(defmethod / [:polar :complex] [^Polar z, ^Complex w]
  (/ z (polar w)))

(defmethod / [:complex :polar] [^Complex z, ^Polar w]
  (/ z (complex w)))

;; --------------------------------------
;; zero?
;; --------------------------------------

(defmethod zero? Complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (and (zero? x) (zero? y))))

(defmethod zero? Polar [^Polar e]
  (= 0 (.radius e)))

;; --------------------------------------
;; ln
;; --------------------------------------

(defmethod ln :complex [^Complex z]
  (Complex. (Math/log (modulus z)) (arg z)))

(defmethod ln :polar [^Polar z]
  (let [ln-mod (Math/log (.radius z))
        radius (Math/sqrt (inc (* ln-mod ln-mod)))]
    (Polar. radius (Math/acos (/ ln-mod radius)))))

;; --------------------------------------
;; exp
;; --------------------------------------

(defmethod exp :complex [^Complex z]
  (let [x (Re z) y (Im z)]
    (Complex. (* (Math/exp x) (Math/cos y))
              (* (Math/exp x) (Math/sin y)))))

(defmethod exp :polar [^Polar z]
  (Complex. (* (.radius z) (Math/cos (.angle z)))
            (-> (* (.radius z) (Math/sin (.angle z))) (mod (* 2 Math/PI)))))

;; --------------------------------------
;; pow
;; --------------------------------------

(defmethod pow [:complex :complex] [^Complex z ^Complex w]
  (exp (* w (ln z))))

;; --------------------------------------
;; Trigonometric Functions
;; --------------------------------------
;; Note that the current implementations of these functions returns their
;; principal values.  For example, many of the inverses have solutions in terms
;; of a quadratic equation, which means they have two solutions; I return only
;; the "+" solution (i.e. disregard the "-" solution).

;; --------------------------------------
;; cos
;; --------------------------------------

(defmethod cos :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (+ (exp I*z) (exp (- I*z)))
       2)))

(defmethod acos :complex [^Complex z]
  (* (- I)
     (ln (+ z (principal-sqrt (- (* z z) 1))))))

;; --------------------------------------
;; sin
;; --------------------------------------

(defmethod sin :complex [^Complex z]
  (let [I*z (* I z)]
    (/ (- (exp I*z) (exp (- I*z)))
       (* 2 I))))

(defmethod asin :complex [^Complex z]
  (* (- I)
     (ln (+ (* I z) (principal-sqrt (- 1 (* z z)))))))

;; --------------------------------------
;; tan
;; --------------------------------------

(defmethod tan :complex [^Complex z]
  (* (/ 1 I)
     (/ (- (exp (* 2 I z)) 1)
        (+ (exp (* 2 I z)) 1))))

(defmethod atan :complex [^Complex z]
  (* (/ 1 (* 2 I))
     (ln (/ (+ 1 (* I z))
            (- 1 (* I z))))))

;; --------------------------------------
;; cot
;; --------------------------------------

(defmethod cot :complex [^Complex z]
  (/ (* I (+ (exp (* 2 I z)) 1))
     (- (exp (* 2 I z)) 1)))

(defmethod acot :complex [^Complex z]
  (* (/ 1 (* 2 I))
     (ln (/ (+ z I)
            (- z I)))))

;; --------------------------------------
;; sec
;; --------------------------------------

(defmethod sec :complex [^Complex z]
  (let [I*z (* I z)]
    (* 2 (+ (exp I*z) (exp (- I*z))))))

(defmethod asec :complex [^Complex z]
  (* (- I) (ln (* 1/4 (+ z (principal-sqrt (- (* z z) 16)))))))

;; --------------------------------------
;; csc
;; --------------------------------------

(defmethod csc :complex [^Complex z]
  (let [I*z (* I z)]
    (* 2 I (- (exp (- I*z)) (exp I*z)))))

(defmethod acsc :complex [^Complex z]
  (* I (ln (* 1/4 (+ (* (- I) z) (principal-sqrt (- 16 (* z z))))))))

;; --------------------------------------
;; sinh
;; --------------------------------------

(defmethod sinh :complex [^Complex z]
  (/ (- (exp z) (exp (- z)))
     2))

(defmethod asinh :complex [^Complex z]
  (ln (+ z (principal-sqrt (+ (* z z) 1)))))

;; --------------------------------------
;; cosh
;; --------------------------------------

(defmethod cosh :complex [^Complex z]
  (/ (+ (exp z) (exp (- z)))
     2))

(defmethod acosh :complex [^Complex z]
  (ln (+ z (principal-sqrt (- (* z z) 1)))))

;; --------------------------------------
;; tanh
;; --------------------------------------

(defmethod tanh :complex [^Complex z]
  (/ (sinh z) (cosh z)))

(defmethod atanh :complex [^Complex z]
  (* (/ 1 2)
     (ln (/ (+ 1 z)
            (- 1 z)))))

;; --------------------------------------
;; csch
;; --------------------------------------

(defmethod csch :complex [^Complex z]
  (* 2 (- (exp (- z)) (exp z))))

(defmethod acsch :complex [^Complex z]
  (- (ln (* 1/4 (+ z (principal-sqrt (+ (* z z) 16)))))))

;; --------------------------------------
;; sech
;; --------------------------------------

(defmethod sech :complex [^Complex z]
  (* 2 (+ (exp (- z)) (exp z))))

(defmethod asech :complex [^Complex z]
  (ln (* 1/4 (+ z (principal-sqrt (- (* z z) 16))))))

;; --------------------------------------
;; coth
;; --------------------------------------

(defmethod coth :complex [^Complex z]
  (/ 1 (tanh z)))

(defmethod acoth :complex [^Complex z]
  (* (/ 1 2)
     (ln (/ (+ z 1)
            (- z 1)))))

;; --------------------------------------
;; print-method
;; --------------------------------------

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

