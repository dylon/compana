(ns compana.real
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=])
  (:use compana.arithmetic))

(defmethod classify Number [^Number _]
  :real)

(defmethod zero? Number [^Number x]
  (= 0 x))

(defmethod zero? clojure.lang.Ratio [^clojure.lang.Ratio x]
  (= 0 (numerator x)))

(defmethod zero? Double [^Double x]
  (< (Math/abs x) ZERO))

(defmethod zero? Float [^Float x]
  (< (Math/abs x) ZERO))

(defmethod abs :real [^Number x]
  (if (< x 0) (- x) x))

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
;;             = ((n - 1) * r_k + x / r_k^(n-1)) / n
;;  6. Let r_(k+1) = r_k + delta. Then, delta = r_(k+1) - r_k
;;     delta = r_(k+1) - r_k
;;           = ((n - 1) * r_k + x / r_k^(n-1)) / n - r_k
;;           = ((n - 1) * r_k + x / r_k^(n-1)) / n - (n * r_k) / n
;;           = ((n - 1) * r_k + x / r_k^(n-1) - n * r_k) / n
;;           = (((n - 1) - n) * r_k + x / r_k^(n-1)) / n
;;           = (x / r_k^(n-1) - r_k) / n
;;  7. Stop once |delta| < precision, where precision ~ 0.
(defmethod principal-nth-root :real [^Number x ^long n]
  (loop [root 1.0]
    (let [delta (/ (- (/ x (Math/pow root (dec n))) root) n)]
      (if (< (Math/abs delta) ZERO) root
        (recur (+ root delta))))))

(defmethod principal-sqrt :real [^Number x]
  (if (< x 0)
    (Math/sqrt (abs x))
    (Math/sqrt x)))

(defmethod < [:real :real] [^Number x, ^Number y]
  (clojure.core/< x y))

(defmethod <= [:real :real] [^Number x, ^Number y]
  (clojure.core/<= x y))

(defmethod = [:real :real] [^Number x, ^Number y]
  (< (abs (- x y)) ZERO))

(defmethod == [:real :real] [^Number x, ^Number y]
  (< (abs (- x y)) ZERO))

(defmethod > [:real :real] [^Number x, ^Number y]
  (clojure.core/> x y))

(defmethod >= [:real :real] [^Number x, ^Number y]
  (clojure.core/>= x y))

(defmethod + [:real :real] [^Number x ^Number y]
  (clojure.core/+ x y))

(defmethod - [:real :real] [^Number x ^Number y]
  (clojure.core/- x y))

(defmethod - [:real :negation] [^Number x]
  (clojure.core/- x))

(defmethod * [:real :real] [^Number x ^Number y]
  (clojure.core/* x y))

(defmethod / [:real :real] [^Number x ^Number y]
  (clojure.core// x y))

(defmethod ln :real [^Number x]
  (Math/log x))

(defmethod exp :real [^Number x]
  (Math/exp x))

(defmethod pow [:real :real] [^Number x ^Number y]
  (Math/pow x y))

(defmethod cos :real [^Number x]
  (Math/cos x))

(defmethod sin :real [^Number x]
  (Math/sin x))

(defmethod tan :real [^Number x]
  (/ (sin x) (cos x)))

(defmethod cot :real [^Number x]
  (/ 1 (tan x)))

(defmethod sec :real [^Number x]
  (/ 1 (cos x)))

(defmethod csc :real [^Number x]
  (/ 1 (sin x)))

(defmethod sinh :real [^Number x]
  (Math/sinh x))

(defmethod cosh :real [^Number x]
  (Math/cosh x))

(defmethod tanh :real [^Number x]
  (/ (sinh x) (cosh x)))
