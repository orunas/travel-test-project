(ns test-project.test-plan3
  (:require
    ;[clojure.test :refer :all]
    ;[test-project.core :as c]
            [test-project.sparql :as s ]
    ; [test-project.test-plan :as t :refer :all]
            [test-project.rdf :as r]
            [test-project.ea :as e]
            [test-project.airport :as air]
            [test-project.ws :as ws]
            [test-project.action :as a]
            ))

; it is simplified version. No conditionals inside methods

(comment                                                    ;examples for cure
  :cue (task :get-home ?beginDate ?endDate)
  :cue (event :insert-bgp ?campaign :rdf:type :tc:PromotionalCampaignType)
  :cue (event :insert-subject-uri :tc ?campaign)

  :task (get-home ?beginDate ?endDate)
  :event (+bgp ?campaign :rdf:type :tc:PromotionalCampaignType)
  :event (+subject-uri :tc ?campaign)
  )

(def actions
  {
   :call-action-generic #(test-project.action/call-action %1 %2)
   ;:call-action-generic #(println "Executing action. uri:" %1 " data:" %2)
   :call-action-test (fn [var1] (print "doing something"))
   }
  )

(def namespaces-prefixes
  {
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :t "http://travelplanning.ex/"
   :s "http://schema.org/"
   :xsd "http://www.w3.org/2001/XMLSchema#"
   :tc "http://travelplanning.ex/Campaign/"
   :ta "http://travelplanning.ex/Airline/"
   :tcn "http://travelplanning.ex/Connection/"
   :tcu "http://travelplanning.ex/ConnectionUpdate/"
   :tcd "http://travelplanning.ex/ConnectionData/"})

(def loc-methods-lib (atom {}) )

(defn add-airport-data [vars]
  (if-let [airport-data (air/get-airport-timezone (s/var-val vars :?airport))]
    (ws/CallWS
      "http://localhost:8087/data"
      (r/rdf namespaces-prefixes
             [(s/var-out vars :?airport)
              :t:AirportLocationLatitude (airport-data :lat)
              :t:AirportLocationLongtitude (airport-data :lng)
              :t:AirportTimezoneUTCOffset (airport-data :gmtOffset)])
      {"Content-Type" "text/turtle; charset=utf-8" })
    (println "error. No data for " (s/var-val vars :?airport))))

(e/def-method
  update-data-on-campaign
  [?campaign]
  ;just a place to modify code to reload
  :test 'nothing1
  ;for event type plans parameters are in event desciption
  ; +bgp does nothing. All events are additions of some information
  :event [?campaign :rdf:type :tc:PromotionalCampaignType]
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition ((?campaign rdf:type tc:PromotionalCampaignType
                            t:campaignAirline ?airline
                            t:campaignSalesStartDate ?salesStart
                            t:campaignSalesEndDate ?salesEnd
                            t:campaignOfferTravelStartDate ?travelStart
                            t:campaignOfferTravelEndDate ?travelEnd)
                  (:filter
                    ; todo change r/now. Sales should be started
                    (s/f-and (s/f< ?salesStart (r/now))
                             (s/f> ?salesEnd (r/now))
                             (s/f< ?salesStart ?salesEnd)
                             )))
  :body
  [#(test-project.task/add-task :check-connections (% :?airline))
   (fn [_] (test-project.task/add-task :task-airport-data))
    #(test-project.task/add-task :update-airline-flights (% :?airline) (% :?travelStart) (% :?travelEnd) (% :?salesStart))
   ])


(e/def-method
  update-connection-when-resentdataexists-method [?airline]
  :task  (:check-connections ?airline)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib       ;don't want to make global var actions
  :precondition ((?connectionData t:CreatedDateTime ?createdTime
                                               tcd:Connection ?connection)
                  (?connection t:ConnectionAirline ?airline)
                  (:filter (s/f> ?createdTime (r/add-days (r/now) -7)))
                  (:order-by (:desc ?createdTime)))         ;we wa
  :body [(fn [_] (println "connection data up to date!"))])

(e/def-method
  update-connection-when-no-recent-data [?airline]
  :task  (:check-connections ?airline)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib        ;don't want to make global var actions
  :precondition (:not-exists (?connectionData tcd:Connection ?connection
                                              t:CreatedDateTime ?createdTime)
                  (?connection t:ConnectionAirline ?airline)
                  (:filter (s/f> ?createdTime (r/add-days (r/now) -7))))
  :body [(fn [_] (test-project.action/action :call-action-generic "http://localhost:8080/flightService/webapi/W6/Connections/" nil))])

(e/def-method
  update-flights-method [?airline ?dateFrom ?dateTo ?oldestOfferDate]
  :task (:update-airline-flights ?airline ?dateFrom ?dateTo ?oldestOfferDate)
  :namespaces namespaces-prefixes :actions actions  :methods loc-methods-lib         ;don't want to make global var actions
  ; find flightdates for airline in provided range, that doesn't have offer information
  :precondition  ([?connection t:ConnectionAirline ?airline
                                t:ConnectionFromAirport ?departureAirport
                                t:ConnectionToAirport ?arrivalAirport
                                ;t:ConnectionFlightDate ?flightDate ; >1 therefore will get specific date
                                t:OperationStartDate ?operationStartDate]
                    (:minus [?offer t:OfferFlight ?flight t:date ?offerDate]
                            [?flight s:arrivalAirport ?arrivalAirport
                                      s:departureAirport ?departureAirport
                                      t:FlightAirline ?airline]
                            (:filter (s/f> ?offerDate ?oldestOfferDate)))
                   (:filter (s/f-and (s/f< ?operationStartDate ?dateTo) (s/f-in ?departureAirport ["<http://travelplanning.ex/Airport/VNO>" "<http://travelplanning.ex/Airport/KUN>" ]))))
  :body [#(test-project.task/add-task
            :update-data-task (% :?airline) (% :?connection) (% :?departureAirport) (% :?arrivalAirport) (% :?dateFrom) (e/apply-val-f r/add-days (% :?dateFrom) 7) (% :?dateTo))
         #(println "update-flights-method middle for:" (s/var-val % :?airline) " " (s/var-val % :?dateFrom) " " (s/var-val % :?dateTo))
         #(test-project.task/add-task
            :update-airline-flights (% :?airline) (% :?dateFrom) (% :?dateTo) (% :?oldestOfferDate))])

(e/def-method
  update-flights-method-done [?airline ?dateFrom ?dateTo ?oldestOfferDate]
  :task (:update-airline-flights ?airline ?dateFrom ?dateTo ?oldestOfferDate)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib       ;don't want to make global var actions
  :precondition  (:not-exists [?connection t:ConnectionAirline ?airline
                                           t:ConnectionFromAirport ?departureAirport
                                           t:ConnectionToAirport ?arrivalAirport
                                           t:OperationStartDate ?operationStartDate]
                             (:minus [?offer t:OfferFlight ?flight t:date ?offerDate]
                                     [?connectionUpdate t:ConnectionUpdateConnection ?connection
                                                         t:RequestEndedDate ?reqEnd]
                                     [?flight s:arrivalAirport ?arrivalAirport
                                              s:departureAirport ?departureAirport
                                              t:FlightAirline ?airline]
                                     (:filter (s/f-and (s/f> ?offerDate ?oldestOfferDate) (s/f> ?reqEnd ?oldestOfferDate) )))
                             (:filter (s/f< ?operationStartDate ?dateTo)))
  :body [(fn [_] (println "Completed updating connections!"))])

(e/def-method
  update-data-by-7-days-method
  [?airline ?fromAirport ?toAirport ?startDate ?iterEndDate ?finalEndDate]

   :task  (:update-data-task ?airline ?connection ?fromAirport ?toAirport ?startDate ?iterEndDate ?finalEndDate )
   :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib       ;don't want to make global var actions
   :precondition ([?fromAirport t:AirportTimezoneUTCOffset ?fromOff]
                   [?toAirport t:AirportTimezoneUTCOffset ?toOff]
                   (:filter (s/f< ?iterEndDate ?finalEndDate))
                   )
   :body [#(test-project.action/action :call-action-generic
             "http://localhost:8080/flightService/webapi/W6/Flights"
             (r/rdf namespaces-prefixes
                    [(r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" (s/var-val % :?airline) (s/var-val % :?fromAirport) (s/var-val % :?toAirport) (r/dateTime-to-id (r/now)))
                     :t:ConnectionUpdateConnection (s/var-out % :?connection)
                     :t:RangeStart (s/var-out (s/datetime-type-to-date % :?startDate))
                     :t:RangeEnd (s/var-out (s/datetime-type-to-date % :?iterEndDate))
                     :t:RequestStartedDate (r/to-rdftype (r/now))
                     :t:FromAirportTimezoneUTCOffset (s/var-out % :?fromOff)
                     :t:ToAirportTimezoneUTCOffset (s/var-out % :?toOff)
                     ]
                    ))
          #(test-project.task/add-task
             :update-data-task (% :?airline) (% :?connection) (% :?fromAirport) (% :?toAirport) (% :?iterEndDate) (e/apply-val-f r/add-days (% :?iterEndDate) 7) (% :?finalEndDate))])

(e/def-method
  update-data-by-7-days-method2
  [?airline ?fromAirport ?toAirport ?startDate ?iterEndDate ?finalEndDate]
  :task  (:update-data-task ?airline ?connection ?fromAirport ?toAirport ?startDate ?iterEndDate ?finalEndDate )
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib        ;don't want to make global var actions
  :precondition ([?fromAirport t:AirportTimezoneUTCOffset ?fromOff]
                  [?toAirport t:AirportTimezoneUTCOffset ?toOff]
                  (:filter (s/f> ?iterEndDate ?finalEndDate)) ; turi but >=
                  )
  :body [#(test-project.action/action :call-action-generic
            "http://localhost:8080/flightService/webapi/W6/Flights"
            (r/rdf namespaces-prefixes
                   [(r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" (s/var-val % :?airline) (s/var-val % :?fromAirport) (s/var-val % :?toAirport) (r/dateTime-to-id (r/now)))
                    :t:ConnectionUpdateConnection (s/var-out % :?connection)
                    :t:RangeStart (s/var-out (s/datetime-type-to-date % :?startDate))
                    :t:RangeEnd (s/var-out (s/datetime-type-to-date % :?finalEndDate))
                    :t:RequestStartedDate (r/to-rdftype (r/now))
                    :t:FromAirportTimezoneUTCOffset (s/var-out % :?fromOff)
                    :t:ToAirportTimezoneUTCOffset (s/var-out % :?toOff)]))])

(e/def-method
  achieve-all-airport-data
  []
  :task (:task-airport-data)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition ([?connection t:ConnectionFromAirport ?airport]
                  (:minus [?airport t:AirportTimezoneUTCOffset ?offset
                           t:AirportLocationLongtitude ?long
                           t:AirportLocationLatitude ?lat]))
  :body [#(add-airport-data %)
         (fn [_] (test-project.task/add-task :task-airport-data))])

(e/def-method
  achieve-all-airport-data-done
  []
  :task (:task-airport-data)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition (:not-exists [?connection t:ConnectionFromAirport ?airport]
                  (:minus [?airport t:AirportTimezoneUTCOffset ?offset
                           t:AirportLocationLongtitude ?long
                           t:AirportLocationLatitude ?lat]))
  :body [(fn [_] (println "task :task-airport-data completed"))])

(def datetime1 (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z") )

(def intention-graph
  {
   :0                                                       ; root element
   {
    :id 0
    :type :root}

  :1                                                        ; event will be mapped to corresponding methods so after root is only method
  {
   :id 1
   :type :method
   :method :update-data-on-campaign
   :?campaign   {:type :uri, :value "00000001", :prefix-ns "http://travelplanning.ex/Campaign/"},
   :?salesStart {:type     :literal,
                 :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                 :value    datetime1}
   :?travelStart {:type     :literal,
                  :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                  :value    datetime1}
   :steps         [:2 :14]
   :parent :0
   }

  :14 {:id 14
       :type :step
       :?campaign   {:type :uri, :value "00000001", :prefix-ns "http://travelplanning.ex/Campaign/"},
       :?salesStart {:type     :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value    datetime1}
       :?travelStart {:type     :literal,
                      :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                      :value    datetime1}
       :body       (fn [_] (print "do something"))
       :parent :1
       }

  ; taks is now
  :2
   {
    :id       2
    :type :task
    ; galetu buti taip jei be content kurio viduje visi paramsai
    :task :update-data-task
    :parameters [{:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}
                 {:type     :literal,
                  :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                  :value    datetime1}]

    ; pvz kaip su content
    :content
    '(task
      {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}
      {:type     :literal,
       :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
       :value    datetime1}
      )
    :parent   :1

    }
   :3
   {:id       3
    :type :method
    :method     :update-data-method,
    :?airline   {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"},
    :?beginDate {:type     :literal,
                 :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                 :value    datetime1},
    :parent   :2
    :steps  [:5 :6]
    }
   :5
   {
    :id 5
    :type :step
    :content
    {
     :?airline   {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"},
     :?beginDate {:type     :literal,
                  :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                  :value    datetime1}
     :body       (fn [_] (print "1st"))
     }
    :parent   :3
    }
   :6
   {
    ;panasiai kaip ir 5 tik nera
    :id 6
    :type :step
    :?airline   {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"},
    :?beginDate {:type     :literal,
                 :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                 :value    datetime1}
    :body       (fn [_] (print "2nd"))
    :parent   :3
    }
   })

(def steps
  {
   :active    #{}
   :normal    #{:5}
   :suspended #{:6 :14}
   })



