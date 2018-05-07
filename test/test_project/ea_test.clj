(ns test-project.ea-test
  (:require [clojure.test :refer :all]
            [test-project.ea :as e]
            [test-project.util :as util]
            [test-project.rdf :as r]
            [test-project.sparql :as s]
    ;[test-project.test-plan2 :as t2]
            [test-project.receive :as rc]
            [clojure.core.logic :as l]))

;********************* setup
(def actions
  {                                                         ;:call-action-generic #(test-project.ea/call-action %1 %2)
   :call-action-generic #(println "Executing action. uri:" %1 " data:" %2)
   :call-action-test (fn [var1] (print "doing something"))
   }
  )


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

(def loc-methods-lib (atom {}) )

(e/def-method
  test-method-for-step-extract
  []
  :task (:some-task)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition nil
  :body [1 2])

(e/def-method
  test-method-2
  []
  :task (:task2)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition nil
  :body [(fn [_] (test-project.ea/add-action :some-action)) ])

(e/def-method
  test-method-1
  []
  :task (:task1)
  :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
  :precondition nil
  :body (list (fn [vars] (test-project.task/add-task :task2)) #(println "something" %)))

(def agenda-0  {:intention-graph {:r0 {:type :root :id :r0}},
                :normal-step-keys []
                :active-step-keys []})


; ************* some code part for manual tests

(comment                                                    ;"code to run in REPL"
  (def a1 (atom et/agenda-0))
  (def f1 (e/add-events-to-agenda @et/loc-methods-lib a1 [{:id 1 :method :test-method-1}]))
  (def f2 (e/progress-in-agenda  et/namespaces-prefixes @et/loc-methods-lib #(first %) a1))
  (def f3 (e/progress-in-agenda  et/namespaces-prefixes @et/loc-methods-lib #(first %) a1))
         )

(comment
  "just run code inside ->>"
  (->>
    (e/add-events-to-agenda @loc-methods-lib (atom agenda-0) [{:id 1 :method :test-method-1}])
    (e/progress-in-agenda t3/namespaces-prefixes @loc-methods-lib #(first %)))

  ; After second run 1st key removed, but added second which points to newly added nodes
  ;
  )


(comment
  (e/add-to-agenda
    @e/methods-lib
    {:stacks []}
    [{:method       :update-data-on-campaign,
      :?campaign    {:type :uri, :value "12345678", :prefix-ns "http://travelplanning.ex/Campaign/"},
      :?salesStart  {:type     :literal,
                     :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                     :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")},
      :?fromAirport {:type :uri, :value "KUN", :prefix-ns "http://travelplanning.ex/Airport/"}}]))


(comment (e/add-events-to-agenda
           @e/methods-lib
           [{:r0 {:type :root :id :r0}} []]
           [{:method       :update-data-on-campaign,
             :?campaign    {:type :uri, :value "12345678", :prefix-ns "http://travelplanning.ex/Campaign/"},
             :?salesStart  {:type     :literal,
                            :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                            :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")},
             :?fromAirport {:type :uri, :value "KUN", :prefix-ns "http://travelplanning.ex/Airport/"}}]))
; ************ actual automated tests

(def t
  (e/extract-steps @loc-methods-lib {:id 1 :method :test-method-for-step-extract}))

(deftest
  test-extract-steps
  (is
    (= (count
           (l/run* [x y]
                   (l/== (e/extract-steps @loc-methods-lib {:id 1 :method :test-method-for-step-extract})
                         [(list {:id 1, :method :test-method-for-step-extract, :steps (list x y), :steps-ordered true}
                                {:id x, :method :test-method-for-step-extract, :parent 1, :type :step, :body 1}
                                {:id y, :method :test-method-for-step-extract, :parent 1, :type :step, :body 2})
                          (list x)])))                      ; tik vienas nes ordered
       1)))



(deftest
  add-event-and-progess-test
  (let [a (atom agenda-0)
        f (e/add-events-to-agenda @loc-methods-lib a [{:id 1 :method :test-method-1}])
        s  (e/progress-in-agenda namespaces-prefixes @loc-methods-lib #(first %) a)]
    (is (= (count (:intention-graph f)) 4)) ;result first (inner) should return 4 nodes (with :r0,  3 added)
    (is (= (count (:intention-graph  s)) 6)) ; second run second 6 (2 added).
    (is (= (count (:normal-step-keys f)) 2))                     ;First run returns 2 keys in :normal-step-keys.
    (is (= (count (:normal-step-keys s)) 2))
    (is (= (second (:normal-step-keys f)) (first (:normal-step-keys s))))
    ))

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

;testas pasenes - neveiks ant grafo
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
