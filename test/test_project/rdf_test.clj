(ns test-project.rdf-test
  (:require [clojure.test :refer :all]
            [test-project.rdf :as r]
            [test-project.sparql :as s]
            ))

(def namespaces-prefixes
  {
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :t "http://travelplanning.ex/"
   :xsd "http://www.w3.org/2001/XMLSchema#"
   :tc "http://travelplanning.ex/Campaign/"
   :ta "http://travelplanning.ex/Airline/"
   :tcn "http://travelplanning.ex/Connection/"
   :tcu "http://travelplanning.ex/ConnectionUpdate/"})

(deftest
  test-rdf
  (is
    (=
      (clojure.string/replace
        "<http://travelplanning.ex/ConnectionUpdate/W6_PLQ_LTN_20180110133850> <http://travelplanning.ex/ConnectionUpdateConnection> <http://travelplanning.ex/Connection/W6_PLQ_LTN>.
         <http://travelplanning.ex/ConnectionUpdate/W6_PLQ_LTN_20180110133850> <http://travelplanning.ex/RequestStartedDate> \"\"\"2018-01-10T13:17:26.277Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime>."
        #"\s+" "")
      (clojure.string/replace
        (#(r/rdf namespaces-prefixes
            [(r/gen-id "http://travelplanning.ex/" "ConnectionUpdate"
                       (s/var-val % :?airline)
                       (s/var-val % :?fromAirport)
                       (s/var-val % :?toAirport)
                       "20180110133850")
             :t:ConnectionUpdateConnection (s/var-out % :?connection)
             :t:RequestStartedDate (r/to-rdftype (java.time.ZonedDateTime/parse "2018-01-10T13:17:26.277Z"))])
          {:?campaign    {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"},
           :?connection  {:type :uri, :value "W6_PLQ_LTN", :prefix-ns "http://travelplanning.ex/Connection/"},
           :?fromAirport {:type :uri, :value "PLQ", :prefix-ns "http://travelplanning.ex/Airport/"},
           :?toAirport   {:type :uri, :value "LTN", :prefix-ns "http://travelplanning.ex/Airport/"},
           :?airline     {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}})
        #"\s+" "")
      ))
  )
