(defproject compana "0.1.0"
  :description "Complex Analysis in Clojure"
  :url "https://github.com/dylon/compana"
  :license {:name "The MIT License"
            :url "http://www.opensource.org/licenses/mit-license.php"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:test {:dependencies [[midje "1.6.0"]]
                    :plugins [[lein-midje "3.1.1"]]}}
  :main compana.main
  :aot [compana.complex.Complex
        compana.complex.Polar])
