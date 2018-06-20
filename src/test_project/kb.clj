(ns test-project.kb
  (:require [test-project.ws :as ws]
            [test-project.json-ld :as jl]
            [clojure.data.json :as json]
            [test-project.jena :as j]))

; saves data to kb. provides functions that wraps kb. users should use clojure structures

(defn add-facts
  "add facts to kb
  params:
  d - data"
  [d]
  (let [dj (-> d jl/context-vars-map-to-json-ld (json/write-str))
        dt (j/read-and-output-model dj "JSON-LD" "TTL")
        ;"application/ld+json; charset=utf-8"
        ct "text/turtle; charset=utf-8"
        ;"http://localhost:3030/Test2"
        url "http://ec2-35-178-111-83.eu-west-2.compute.amazonaws.com:10035/repositories/Flights/statements"
        ]
     (ws/CallWS url dt {"Content-Type" ct, "Authorization" "Basic ZWE6am9wbDE="})
    )
  )

(comment
  [?connection :t:ConnectionAirline ?airline
   :t:ConnectionFromAirport ?departureAirport
   :t:ConnectionToAirport ?arrivalAirport]
  [?sync :ts:connection-sync ?connection-sync]
  [?connection-sync :ts:connection ?connection :ts:synced ?synced] :filter (= ?synced false)

  )
(def ?sync "http://testtravel.ex/12345")

(comment
  (insert
    [?sync :ts:connection-sync ?connection-sync]
    [?connection-sync :ts:connection ?connection :ts:synced ?synced])
  (select ([?connection t:ConnectionAirline ?airline
     t:ConnectionFromAirport ?departureAirport
     t:ConnectionToAirport ?arrivalAirport
     ;t:ConnectionFlightDate ?flightDate ; >1 therefore will get specific date
     t:OperationStartDate ?operationStartDate]
     (:minus
       [?offer t:OfferFlight ?flight t:date ?offerDate]
       [?flight s:arrivalAirport ?arrivalAirport
        s:departureAirport ?departureAirport
        t:FlightAirline ?airline]
       (:filter (s/f> ?offerDate ?oldestOfferDate)))
     (:filter (s/f-and (s/f< ?operationStartDate ?dateTo) (s/f-in ?departureAirport ["<http://travelplanning.ex/Airport/VNO>" "<http://travelplanning.ex/Airport/KUN>"]))))))
