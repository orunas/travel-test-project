(ns test-project.kb)
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
