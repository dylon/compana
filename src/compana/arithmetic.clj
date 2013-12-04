(ns compana.arithmetic
  (:refer-clojure :exclude [zero? + - * /]))

(def ^:const ZERO 1e-12)

(defmulti classify class)
(defmethod classify :default [_] :real)

(defmulti nth-roots
  "Finds all n roots of the complex number if it is not zero, or [zero] if it is."
  (fn [x ^long n] (classify x)))

(defmulti principal-nth-root
  "Returns the nth root of a non-negative, real number."
  (fn [x ^long n] {:pre [(>= x 0) (pos? n)]} (classify x)))

(defmulti +
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod + :variadic [x y & others]
  (apply + (+ x y) others))

(defmulti -
  (fn
    ([x] [(classify x) :negation])
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod - :variadic [x y & others]
  (apply - (- x y) others))

(defmulti *
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & others] :variadic)))

(defmethod * :variadic [x y & others]
  (apply * (* x y) others))

(defmulti /
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & others] :variadic)))

(defmethod / :variadic [x y & others]
  (apply / (/ x y) others))

(defmulti zero? class)

(defmulti ln classify)

(defmulti exp classify)

(defmulti pow
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & args] :variadic)))

(defmethod pow :variadic [x & args]
  (pow x (apply pow args)))

(defmulti cos classify)

(defmulti sin classify)
