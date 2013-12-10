(ns compana.real_test
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=])
  (:use [compana arithmetic real]
        midje.sweet))

(fact "a op e = e op a = a, where e is the identity under op"
  (tabular
    (fact
      (?op ?lhs ?rhs) => 2)
      ?op ?lhs ?rhs
       +    2    0
       +    0    2
       *    2    1
       *    1    2)
  (tabular
    (fact
      (?op ?lhs ?rhs) => 1/3)
      ?op ?lhs ?rhs
       +   1/3   0
       +    0   1/3
       *   1/3   1
       *    1   1/3)
  (tabular
    (fact
      (?op ?lhs ?rhs) => 2.0)
      ?op ?lhs ?rhs
       +   2.0  0.0
       +   0.0  2.0
       *   2.0  1.0
       *   1.0  2.0))

(fact "a op a' = e, where a' is the inverse of a and e is the identity under op"
  (+ 2 (- 2)) => 0
  (+ 1/3 (- 1/3)) => 0
  (+ 2.0 (- 2.0)) => 0.0
  (* 2 (/ 1 2)) => 1
  (* 1/3 (/ 1 1/3)) => 1
  (* 2.0 (/ 1.0 2.0)) => 1.0)

(fact "a'' = (a')' = a, where a' is the inverse of a"
  (- (- 2)) => 2
  (- (- 1/3)) => 1/3
  (- (- 2.0)) => 2.0
  (/ 1 (/ 1 2)) => 2
  (/ 1 (/ 1 1/3)) => 1/3
  (/ 1 (/ 1 2.0)) => 2.0
  (ln (exp 2)) => 2.0
  (ln (exp 1/3)) => (double 1/3)
  (ln (exp 2.0)) => 2.0
  (pow (pow 2 -1) -1) => 2.0
  (pow (pow 1/3 -1) -1) => (double 1/3)
  (pow (pow 2.0 -1.0) -1.0) => 2.0)

