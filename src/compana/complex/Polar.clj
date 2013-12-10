(ns compana.complex.Polar
  (:gen-class
   :name compana.complex.Polar
   :extends java.lang.Number
   :init init
   :state state
   :constructors {[Number Number] []}
   :methods [[radius [] Number]
             [angle [] Number]
             [real [] Number]
             [imaginary [] Number]]
   :main false))

(defn -init
  ([^Number radius, ^Number angle]
   [[] {:radius radius, :angle angle}]))

(defn -radius
  ([^compana.complex.Polar this]
   (:radius (.state this))))

(defn -angle
  ([^compana.complex.Polar this]
   (:angle (.state this))))

(defn -real
  ([^compana.complex.Polar this]
   (* (.radius this) (Math/cos (.angle this)))))

(defn -imaginary
  ([^compana.complex.Polar this]
   (* (.radius this) (Math/sin (.angle this)))))

(defn -doubleValue
  ([^compana.complex.Polar this]
   (double (.real this))))

(defn -floatValue
  ([^compana.complex.Polar this]
   (float (.real this))))

(defn -intValue
  ([^compana.complex.Polar this]
   (int (.real this))))

(defn -longValue
  ([^compana.complex.Polar this]
   (long (.real this))))

