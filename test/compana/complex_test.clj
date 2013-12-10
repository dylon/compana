(ns compana.complex_test
  (:refer-clojure :exclude [zero? + - * / < = == > <= >=])
  (:use [compana arithmetic real complex]
        midje.sweet)
  (:import [compana.complex Complex]))

(def ^:const PI Math/PI)

(defn approx
  ([^Complex z]
   (fn ([^Complex w] (== z w)))))

(fact
  (cos (complex (/ PI 3))) => (approx (complex 0.5))
  (acos (complex 0.5)) => (approx (complex (/ PI 3)))
  (acos (cos (complex (/ PI 3)))) => (approx (complex (/ PI 3))))

