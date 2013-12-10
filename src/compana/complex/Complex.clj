(ns compana.complex.Complex
  (:gen-class
   :name compana.complex.Complex
   :extends java.lang.Number
   :init init
   :state state
   :constructors {[Number Number] []}
   :methods [[real [] Number]
             [imaginary [] Number]]
   :main false))

(defn -init
  ([^Number real, ^Number imaginary]
   [[] {:real real, :imaginary imaginary}]))

(defn -real
  ([^compana.complex.Complex this]
   (:real (.state this))))

(defn -imaginary
  ([^compana.complex.Complex this]
   (:imaginary (.state this))))

(defn -doubleValue
  ([^compana.complex.Complex this]
   (double (.real this))))

(defn -floatValue
  ([^compana.complex.Complex this]
   (float (.real this))))

(defn -intValue
  ([^compana.complex.Complex this]
   (int (.real this))))

(defn -longValue
  ([^compana.complex.Complex this]
   (long (.real this))))

