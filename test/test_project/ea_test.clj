(ns test-project.ea-test
  (:require [clojure.test :refer :all]
            [test-project.ea :as e]
            [test-project.util :as util]
            [test-project.rdf :as r]
            [test-project.sparql :as s]
    ;[test-project.test-plan2 :as t2]
            [test-project.receive :as rc]))

;********************* setup
(def actions
  {                                                         ;:call-action-generic #(test-project.ea/call-action %1 %2)
   :call-action-generic #(println "Executing action. uri:" %1 " data:" %2)})

(def namespaces-prefixes
  {
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :t "http://travelplanning.ex/"
   :xsd "http://www.w3.org/2001/XMLSchema#"
   :tc "http://travelplanning.ex/Campaign/"
   :ta "http://travelplanning.ex/Airline/"
   :tcn "http://travelplanning.ex/Connection/"
   :tcu "http://travelplanning.ex/ConnectionUpdate/"
   :tcd "http://travelplanning.ex/ConnectionData/"})

(def l-methods-lib (atom {}) )

(e/def-method
  update-data-on-campaign
  [?campaign]
  ;just a place to modify code to reload
  :test 'nothing1
  ;for event type plans parameters are in event desciption
  ; +bgp does nothing. All events are additions of some information
  :event [?campaign :rdf:type :tc:PromotionalCampaignType]
  :namespaces namespaces-prefixes :actions actions :methods l-methods-lib
  :precondition ((?campaign rdf:type tc:PromotionalCampaignType
                            t:campaignAirline ?airline
                            t:campaignSalesStartDate ?salesStart
                            t:campaignSalesEndDate ?salesEnd
                            t:campaignOfferTravelStartDate ?travelStart
                            t:campaignOfferTravelEndDate ?travelEnd)
                  (?connection
                    t:ConnectionAirline ?airline
                    t:ConnectionFromAirport ?fromAirport
                    t:ConnectionToAirport ?toAirport)
                  (:filter
                    ; todo change r/now. Sales should be started
                    (s/f-and (s/f< ?salesStart (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z"))
                             ; (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                             ;(s/f< ?salesStart ?dateEnd)
                             (s/f< ?salesStart ?salesEnd)
                             (s/f-in ?fromAirport #{"<http://travelplanning.ex/Airport/KUN>" "<http://travelplanning.ex/Airport/VNO>" "<http://travelplanning.ex/Airport/PLQ>"})
                             )))
  :body
  [true
   [(fn [vars]
      (test-project.task/add-task :update-data-task (vars :?airline) (vars :?endDate) (r/add-days (s/var-val vars :?endDate) 7) (vars :?salesStart) (vars :?salesEnd)))]])


(e/def-method
  update-connection-test-method
  [?airline]
  :task  (:update-connection-task ?airline)
  :namespaces namespaces-prefixes :actions actions   :methods l-methods-lib      ;don't want to make global var actions
  :precondition (:not-exists
                  (?connectionData t:CreatedDateTime ?createdTime
                                   t:Connections ?connection)
                  (?connection t:ConnectionAirline ?airline)
                  (:filter (s/f< ?createdTime (r/add-days (r/now) -7)))) ;we wa
  :body [true [#(print "doing something")]])

(e/def-method
         update-data-on-campaign-salesStart
         [?campaign ?salesStart]
         ;for event type plans parameters are in event desciption
         ; +bgp does nothing. All events are additions of some information
         :event [?campaign :t:campaignSalesStartDate ?salesStart]
         :namespaces namespaces-prefixes :actions actions  :methods l-methods-lib
         :precondition ((?campaign  rdf:type tc:PromotionalCampaignType
                                    t:campaignAirline ?airline
                                    t:campaignSalesStartDate ?salesStart
                                    t:campaignSalesEndDate ?salesEnd
                                    t:campaignOfferTravelStartDate ?travelStart
                                    t:campaignOfferTravelEndDate ?travelEnd)
                         (?connection
                           t:ConnectionAirline ?airline
                           t:ConnectionFromAirport ?fromAirport
                           t:ConnectionToAirport ?toAirport)
                         (:filter
                           (s/f-and (s/f< ?salesStart (r/now))
                                    ; (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                                    ;(s/f< ?salesStart ?dateEnd)
                                    (s/f< ?salesStart ?salesEnd)
                                    (s/f-in
                                      ?fromAirport
                                      #{"<http://travelplanning.ex/Airport/KUN>"
                                        "<http://travelplanning.ex/Airport/VNO>"
                                        "<http://travelplanning.ex/Airport/PLQ>"}))))
         :body
         [true
          (list (fn [vars]
                  ((actions :call-action-generic)
                    "http://localhost:8080/flightService/webapi/W6"
                    (r/rdf namespaces-prefixes
                           [(r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" (s/var-val vars :?airline) (s/var-val vars :?fromAirport) (s/var-val vars :?toAirport) (r/dateTime-to-id (r/now)))
                            :t:ConnectionUpdateConnection (str "t:Connection/" (s/var-val vars :?connection))
                            ;  :t:RangeStart (s/var-out (s/datetime-type-to-date vars :?beginDate))
                            ; :t:RangeEnd (s/var-out (s/datetime-type-to-date vars :?endDate))
                            :t:RequestStartedDate (r/to-rdftype (r/now))])))
                (fn [vars]
                  (print "just something")))])



; ************ actual tests

(deftest
  test-process-precondition-query-results
  (is
    (=
      {:?campaign   {:type :uri, :value "00000001", :prefix-ns "http://travelplanning.ex/Campaign/"},
       :?salesStart {:type     :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value    (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")}
       :?travelStart {:type     :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value    (java.time.ZonedDateTime/parse "2017-11-20T00:00:01.39Z")}
       :?connection  {:type :uri, :value "W6_PLQ_LTN" :prefix-ns "http://travelplanning.ex/Connection/"}}
      (rc/add-precondition-query-results-to-context
        {:?campaign   {:type :uri, :value "00000001", :prefix-ns "http://travelplanning.ex/Campaign/"},
         :?salesStart {:type     :literal,
                       :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                       :value    (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")}}
        [{:travelStart {:type     "literal",
                        :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                        :value    "2017-11-20T00:00:01.39Z"},
          :connection  {:type "uri", :value "http://travelplanning.ex/Connection/W6_PLQ_LTN"}}])
      )))

(deftest
  test-find-all-relevant-methods-for-all-events
  (is (=
        [(list
           {:method :update-data-on-campaign-salesStart,
            :?campaign {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"},
            :?salesStart {:type :literal, :datatype "http://www.w3.org/2001/XMLSchema#dateTime",:value (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")}}
           {:method    :update-data-on-campaign,
            :?campaign {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"}})]
        (rc/find-all-relevant-methods-for-all-events
          ;shoud should be loaded
          (vals @l-methods-lib)
          ["PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \nPREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
          PREFIX t: <http://travelplanning.ex/>\nPREFIX ta: <http://travelplanning.ex/Airport/>\nPREFIX tc: <http://travelplanning.ex/Campaign/>
          PREFIX te: <http://travelplanning.ex/Event/>\nPREFIX et: <http://travelplanning.ex/Transaction/>
          PREFIX tt: <http://travelplanning.ex/TransactionTriple/>\nprefix tair: <http://travelplanning.ex/Airline/>
          tc:123456999 rdf:type tc:PromotionalCampaignType;\n  t:campaignAirline tair:W6;
           t:campaignSalesStartDate \"\"\"2017-10-16T00:00:01.390Z\"\"\"^^xsd:dateTime;
             t:campaignSalesEndDate \"\"\"2017-10-19T00:00:01.390Z\"\"\"^^xsd:dateTime ;
               t:campaignOfferTravelStartDate \"\"\"2017-11-20T00:00:01.390Z\"\"\"^^xsd:dateTime ;
                 t:campaignOfferTravelEndDate \"\"\"2017-12-20T00:00:01.390Z\"\"\"^^xsd:dateTime."]))))


(deftest
  test-method-update-data-on-campaign-precondition
  (is
    (=
      (clojure.string/replace "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX t: <http://travelplanning.ex/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX tc: <http://travelplanning.ex/Campaign/>
PREFIX ta: <http://travelplanning.ex/Airline/>
PREFIX tcn: <http://travelplanning.ex/Connection/>
PREFIX tcu: <http://travelplanning.ex/ConnectionUpdate/>
PREFIX tcd: <http://travelplanning.ex/ConnectionData/>
SELECT
?toAirport ?salesStart ?fromAirport ?travelEnd ?salesEnd ?travelStart ?airline ?connection
 WHERE {
 <http://travelplanning.ex/Campaign/123456999> rdf:type tc:PromotionalCampaignType.
 <http://travelplanning.ex/Campaign/123456999> t:campaignAirline ?airline.
 <http://travelplanning.ex/Campaign/123456999> t:campaignSalesStartDate ?salesStart.
 <http://travelplanning.ex/Campaign/123456999> t:campaignSalesEndDate ?salesEnd.
 <http://travelplanning.ex/Campaign/123456999> t:campaignOfferTravelStartDate ?travelStart.
 <http://travelplanning.ex/Campaign/123456999> t:campaignOfferTravelEndDate ?travelEnd.
 ?connection t:ConnectionAirline ?airline.
 ?connection t:ConnectionFromAirport ?fromAirport.
 ?connection t:ConnectionToAirport ?toAirport.
  FILTER ((?salesStart<\"\"\"2017-10-16T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime>) &&
   (?salesStart<?salesEnd) &&
    (?fromAirport in (<http://travelplanning.ex/Airport/KUN>,<http://travelplanning.ex/Airport/VNO>,<http://travelplanning.ex/Airport/PLQ>)))}\n LIMIT 1"
                              #"\s+" "")
      (clojure.string/replace
        ((((@l-methods-lib :update-data-on-campaign) :precondition) :query)
          {:method      :update-data-on-campaign-salesStart,
           :?campaign   {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"},
           :?salesStart {:type     :literal,
                         :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                         :value    (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")}})
        #"\s+" ""))))

(defn first-select [v]
  (first v))

(deftest
  test-process-body-item2times-until-empty
  (is
    (->> (e/process-body-item
           @l-methods-lib
           first-select
           (list {:method           :update-data-on-campaign-salesStart,
                  :?campaign        {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"},
                  :?salesStart      {:type     :literal,
                                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                                     :value    (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")},
                  :?fromAirport     {:type :uri, :value "KUN", :prefix-ns "http://travelplanning.ex/Airport/"},
                  :?toAirport       {:type :uri, :value "LTN", :prefix-ns "http://travelplanning.ex/Airport/"},
                  :?airline         {:type :uri, :value "W", :prefix-ns "http://travelplanning.ex/Airline/"}
                  :unprocessed-body ((@l-methods-lib :update-data-on-campaign-salesStart) :body)}))
         (e/process-body-item @l-methods-lib first-select)
         (empty?))))


;however this is not correct unit test since it depends on external db
(deftest test-not-exist-precondition-eval
  (is (=
        {
         :method :update-connection-test-method,
         :?airline {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}}
        (rc/eval-precondition {:method   :update-connection-test-method,
                               :?airline {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}}
                              (@test-project.ea-test/l-methods-lib :update-connection-test-method)))) )

(deftest test-remove-undanlder-events
  (is
    (=
      ()
      (rc/filter-and-remove-unhandled-events [1 2] [nil]))))



(deftest test-build-precondintion-with-minus-negation
  (is
    (=
      (util/filter-out-whitespaces "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nPREFIX t: <http://travelplanning.ex/>\nPREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\nPREFIX tc: <http://travelplanning.ex/Campaign/>\nPREFIX ta: <http://travelplanning.ex/Airline/>\nPREFIX tcn: <http://travelplanning.ex/Connection/>\nPREFIX tcu: <http://travelplanning.ex/ConnectionUpdate/>\nPREFIX tcd: <http://travelplanning.ex/ConnectionData/>
  SELECT
 ?operationStartDate ?departureAirport ?arrivalAirport ?connection
  WHERE {
 ?connection t:ConnectionAirline <http://travelplanning.ex/Airline/W6>.
 ?connection t:ConnectionFromAirport ?departureAirport.
 ?connection t:ConnectionToAirport ?arrivalAirport.
 ?connection t:OperationStartDate ?operationStartDate.
  MINUS { ?offer t:OfferFlight ?flight.
 ?offer t:date ?offerDate.
 ?flight s:arrivalAirport ?arrivalAirport.
 ?flight s:departureAirport ?departureAirport.
 ?flight t:FlightAirline <http://travelplanning.ex/Airline/W6>.
 ?flight s:departureTime ?departureTime.
  FILTER (?offerDate < \"\"\"2017-10-10T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime>) }
 FILTER (?operationStartDate < \"\"\"2017-11-10T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime> )}
 LIMIT 1")
      (util/filter-out-whitespaces ((:query (s/build-precondition namespaces-prefixes #{?airline ?olderThanDate ?dateTo} ([?connection t:ConnectionAirline ?airline t:ConnectionFromAirport ?departureAirport
                                                                                       t:ConnectionToAirport ?arrivalAirport t:OperationStartDate ?operationStartDate]
                                                                                       (:minus [?offer t:OfferFlight ?flight t:date ?offerDate]
                                                                                         [?flight s:arrivalAirport ?arrivalAirport s:departureAirport ?departureAirport
                                                                                          t:FlightAirline ?airline s:departureTime ?departureTime]
                                                                                         (:filter (s/f< ?offerDate ?olderThanDate)))
                                                                                       ;; gerai tik ciklas
                                                                                       (:filter (s/f< ?operationStartDate ?dateTo)))))
         {:method         :update-data-on-campaign-salesStart,
          :?airline       {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"},
          :?olderThanDate {:type     :literal,
                           :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                           :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")},
          :?dateTo {:type     :literal,
                           :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                           :value    (java.time.ZonedDateTime/parse "2017-11-10T00:00:01.390Z")},
          :?fromAirport   {:type :uri, :value "KUN", :prefix-ns "http://travelplanning.ex/Airport/"}})))))

(comment
  (e/add-to-agenda
    @e/methods-lib
    {:stacks []}
    [{:method       :update-data-on-campaign-salesStart,
      :?campaign    {:type :uri, :value "12345678", :prefix-ns "http://travelplanning.ex/Campaign/"},
      :?salesStart  {:type     :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")},
      :?fromAirport {:type :uri, :value "KUN", :prefix-ns "http://travelplanning.ex/Airport/"}}]))