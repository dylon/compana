(ns compana.arithmetic
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=]))

(def ^:const ZERO 1e-12)

(declare zero? + - * / < = == > <= >=)

(defmulti classify
  (fn ([x] (class x))))

(defmulti abs classify)

(defmulti nth-roots
  "Finds all n roots of the complex number if it is not zero, or [zero] if it is."
  (fn [x ^long n] (classify x)))

(defmulti principal-nth-root
  "Returns the nth root of a non-negative, real number."
  (fn [x ^long n] {:pre [(>= x 0) (pos? n)]} (classify x)))

(defmulti principal-sqrt classify)

(defmulti <
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod < :singleton
  ([_]
   true))

(defmethod < :variadic
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defmulti =
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod = :singleton
  ([_]
   true))

(defmethod = :variadic
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))

(defmulti ==
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod == :singleton
  ([_]
   true))

(defmethod == :variadic
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defmulti <=
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod <= :singleton
  ([_]
   true))

(defmethod <= :variadic
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defmulti >
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod > :singleton
  ([_]
   true))

(defmethod > :variadic
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defmulti >=
  (fn
    ([x] :singleton)
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod >= :singleton
  ([_]
   true))

(defmethod >= :variadic
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defmulti +
  (fn
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

(defmethod + :variadic [x y & others]
  (apply + (+ x y) others))

(defmulti -
  (fn
    ([x] [(classify x) :negation])
    ([x y] [(classify x) (classify y)])
    ([x y & more] :variadic)))

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
    ([x y & more] :variadic)))

(defmethod pow :variadic [x & more]
  (pow x (apply pow more)))

(defmulti cos classify)

(defmulti acos classify)

(defmulti sin classify)

(defmulti asin classify)

(defmulti tan classify)

(defmulti atan classify)

(defmulti cot classify)

(defmulti acot classify)

(defmulti sec classify)

(defmulti asec classify)

(defmulti csc classify)

(defmulti acsc classify)

(defmulti sinh classify)

(defmulti asinh classify)

(defmulti cosh classify)

(defmulti acosh classify)

(defmulti tanh classify)

(defmulti atanh classify)

(defmulti csch classify)

(defmulti acsch classify)

(defmulti sech classify)

(defmulti asech classify)

(defmulti coth classify)

(defmulti acoth classify)
