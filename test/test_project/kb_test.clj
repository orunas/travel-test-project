(ns test-project.kb-test
  (:require [clojure.test :refer :all]
            [test-project.util :as u]
            [test-project.rdf :as r]
            [test-project.kb :as kb]
            [test-project.json-ld :as jl]
            [test-project.context :as ctx]))

   (def vars {:?airline
              {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}})

(defn fts [v]
  {
               :id           {:type :uri :value (u/dateTime-to-id (r/now)) :prefix-ns "http://travelplanning.ex/Request/"}
               :url          {:type :string :value "http://localhost:8080/flightService/webapi/W6/Flights"}
               :method       (jl/http-methods :get)
               :query-params {
                              :id      (ctx/time-id "http://travelplanning.ex/ConnectionUpdate/" (ctx/var-val v :?airline))
                              :airline (v :?airline) }
               })

  (comment kb/add-facts
    (fts vars ) )

