(ns test-project.test-plan
  (:require [test-project.rdf :as r] [test-project.core :as c] [test-project.sparql :as s] :reload)
 )

; is it ok that here is with ?
(def variables-defs
  {
   :?campaign    {:type "uri" :prefix "http://travelplanning.ex/Campaign/"}
   :campaign    {:type "uri" :prefix "http://travelplanning.ex/Campaign/"}
   :?connection  {:type "uri" :prefix "http://travelplanning.ex/Connection/"}
   :?airline     {:type "uri" :prefix "http://travelplanning.ex/Airline/"}
   :?fromAirport {:type "uri" :prefix "http://travelplanning.ex/Airport/"}
   :?toAirport   {:type "uri" :prefix "http://travelplanning.ex/Airport/"}
   :?travelStart {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   :?travelEnd   {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   :?salesStart  {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   :?salesEnd    {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   :?beginDate {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   :?endDate  {:type "literal" :datatype "http://www.w3.org/2001/XMLSchema#dateTime"}
   })

(def get-home-method
  {
   :name         :get-home-method
   :cue          '(task :get-home ?beginDate ?endDate)
   :precondition nil
   :body         '(true ((print "doing... get-home \n")))
   })

(def namespaces-prefixes
  {
    :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    :t "http://travelplanning.ex/"
    :xsd "http://www.w3.org/2001/XMLSchema#"
    :tc "http://travelplanning.ex/Campaign/"
    :ta "http://travelplanning.ex/Airline/"
    :tcu "http://travelplanning.ex/ConnectionUpdate/"})

(def update-data-on-campaign
  {
   :name         :update-data-on-campaign
   :priority     :medium
   :namespaces   '[rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                   t "http://travelplanning.ex/"
                   xsd "http://www.w3.org/2001/XMLSchema#"]
   :parameters   '[?campaign]
   :cue          '(c/validate-event #"http:\/\/travelplanning\.ex\/Campaign\/\d+" :?campaign :Insert)
   :precondition '(c/query *vars* (format "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                   PREFIX t: <http://travelplanning.ex/>
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                prefix tc:    <http://travelplanning.ex/Campaign/>

                select ?airline ?salesStart ?travelStart ?travelEnd ?connection ?toAirport ?fromAirport
                where {
                  %s rdf:type tc:PromotionalCampaignType;
                    t:campaignAirline ?airline;
                    t:campaignSalesStartDate ?salesStart;
                    t:campaignSalesEndDate ?salesEnd ;
                    t:campaignOfferTravelStartDate ?travelStart ;
                    t:campaignOfferTravelEndDate ?travelEnd .
                   ?connection t:ConnectionAirline ?airline;
                    t:ConnectionFromAirport ?fromAirport ;
                    t:ConnectionToAirport ?toAirport .
                 FILTER (( ?salesStart < %s) && (?salesEnd > %s))  }
                  Limit 1" (t (str "Campaign/" (*vars* :?campaign))) (r/to-rdftype (r/now)) (r/to-rdftype (r/now))))

   :body         '(true
                    ((test-project.core/call-action
                       "http://localhost:8080/flightService/webapi/W6"
                       (r/rdf
                         (r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" (*vars* :?airline) (*vars* :?fromAirport) (*vars* :?toAirport) (r/dateTime-to-id (r/now)))
                         (t :ConnectionUpdateConnection)
                         (t (str "Connection/" (*vars* :?connection)))
                         (r/rei
                           (t :RangeStart) (r/literal (r/dateTime-to-date (*vars* :?travelStart)) :type (xsd :date))
                           (t :RangeEnd) (r/literal (r/dateTime-to-date (r/add-days (*vars* :?travelStart) 7)) :type (xsd :date))
                           (t :RequestStartedDate) (r/to-rdftype (r/now)))))
                      (test-project.core/add-task :get-home (r/now) (*vars* :?travelStart) )
                      (print "doing... 2 ne kazka \n")
                      ))})

(def update-data-on-campaign-2
  {
   :name         :update-data-on-campaign-2
   :cue          '(c/validate-event #"http:\/\/travelplanning\.ex\/Campaign\/\d+" :?campaign :Insert)
   :precondition '(c/query test-project.core/*vars* (format "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                   PREFIX t: <http://travelplanning.ex/>
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                prefix tc:    <http://travelplanning.ex/Campaign/>

                select ?airline ?salesStart ?salesEnd ?travelStart ?travelEnd
                where {
                  %s rdf:type tc:PromotionalCampaignType;
                    t:campaignAirline ?airline;
                    t:campaignSalesStartDate ?salesStart;
                    t:campaignSalesEndDate ?salesEnd ;
                    t:campaignOfferTravelStartDate ?travelStart ;
                    t:campaignOfferTravelEndDate ?travelEnd .
                 FILTER (( ?salesStart < %s) && (?salesEnd > %s))  }
                  Limit 1" (t (str "Campaign/" ?campaign)) (r/to-rdftype (r/now)) (r/to-rdftype (r/now))))

   :body         '(true
                    ((test-project.core/add-task
                       :update-data-task ?airline
                       ?travelStart (r/add-days ?travelStart 7)
                       ?travelStart ?travelEnd)
                      ))})

(def update-data-by-7-days-method
  {
   :name         :update-data-by-7-days-method
   :cue          '(task :update-data-task ?airline ?beginDate ?endDate ?salesStart ?salesEnd)
   :precondition '(c/query test-project.core/*vars* (format "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                   PREFIX t: <http://travelplanning.ex/>
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                prefix tc:    <http://travelplanning.ex/Campaign/>

                select ?connection ?fromAirport ?toAirport
                where {
                   ?connection t:ConnectionAirline %s ;
                   t:ConnectionFromAirport ?fromAirport ;
                   t:ConnectionToAirport ?toAirport .
                   }
                  Limit 1" (t (str "Airline/" ?airline))))
   :body         '((c/before ?endDate ?salesEnd)
                    (
                      (test-project.core/call-action
                       "http://localhost:8080/flightService/webapi/W6"
                       (r/rdf
                         (r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" ?airline ?fromAirport ?toAirport (r/dateTime-to-id (r/now)))
                         (t :ConnectionUpdateConnection)
                         (t (str "Connection/" ?connection))
                         (r/rei
                           (t :RangeStart) (r/literal (r/dateTime-to-date ?beginDate) :type (xsd :date))
                           (t :RangeEnd) (r/literal (r/dateTime-to-date ?endDate) :type (xsd :date))
                           (t :RequestStartedDate) (r/to-rdftype (r/now)))))
                      (test-project.core/add-task
                        :update-data-task ?airline
                        ?endDate (r/add-days ?endDate 7)
                        ?salesStart ?salesEnd))
                    (true)
                      ((test-project.core/call-action
                         "http://localhost:8080/flightService/webapi/W6"
                         (r/rdf
                           (r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" ?airline ?fromAirport ?toAirport (r/dateTime-to-id (r/now)))
                           (t :ConnectionUpdateConnection)
                           (t (str "Connection/" ?connection))
                           (r/rei
                             (t :RangeStart) (r/literal (r/dateTime-to-date ?beginDate) :type (xsd :date))
                             (t :RangeEnd) (r/literal (r/dateTime-to-date ?salesEnd) :type (xsd :date))
                             (t :RequestStartedDate) (r/to-rdftype (r/now)))))))})

; we try to rewrite each statement as function
(comment
  (fn [vars variables-defs]
    "function for statement (c/before ?endDate ?salesEnd)"
    (c/before (vars :endDate) (vars :salesEnd))
    )
  ;  (test-project.core/call-action ....
  (defn statement2 [vars variables-defs]
    (test-project.core/call-action
      "http://localhost:8080/flightService/webapi/W6"
      (r/rdf
        (r/gen-id "http://travelplanning.ex/" "ConnectionUpdate"
                  (vars :airline)
                  (vars :fromAirport)
                  (vars :toAirport)
                  (r/dateTime-to-id (r/now)))
        (t :ConnectionUpdateConnection)
        (t (str "Connection/" (vars :connection)))
        (r/rei
          (t :RangeStart) (r/literal (r/dateTime-to-date (vars :beginDate)) :type (xsd :date))
          (t :RangeEnd) (r/literal (r/dateTime-to-date (vars :endDate)) :type (xsd :date))
          (t :RequestStartedDate) (r/to-rdftype (r/now))))))


  )


(def plans
  {
   ;:update-data-on-campaign update-data-on-campaign
   :get-home-method get-home-method
   :update-data-on-campaign-2 update-data-on-campaign-2
   :update-data-by-7-days-method update-data-by-7-days-method
   })
(comment
  (defmethod name
    [& params]
    :cue (event )
    :pre a-query

    :body ()

    ))

(def a-query2filter
  '(s/and
    (s/< ?salesStart (r/now))
    (s/> ?salesEnd (r/add-days ?dateEnd 1))
    (s/< ?salesStart ?dateEnd)
    (s/< ?salesStart ?salesEnd)
    (s/in ?fromAirport #{"<http://travelplanning.ex/Airport/VNO>",
                         "<http://travelplanning.ex/Airport/KUN>", ;<-
                         "<http://travelplanning.ex/Airport/PLQ>"})))

(def a-query2a '{:where
                        ([?campaign
                          rdf:type tc:PromotionalCampaignType
                          t:campaignAirline ?airline
                          t:campaignSalesStartDate ?salesStart
                          t:campaignSalesEndDate ?salesEnd
                          t:campaignOfferTravelStartDate ?travelStart
                          t:campaignOfferTravelEndDate ?travelEnd]
                          [?connection
                           t:ConnectionAirline ?airline
                           t:ConnectionFromAirport ?fromAirport
                           t:ConnectionToAirport ?toAirport]
                          :filter
                          (s/and
                            (s/< ?salesStart (r/now))
                            (s/> ?salesEnd (r/add-days ?dateEnd 1))
                            (s/< ?salesStart ?dateEnd)
                            (s/< ?salesStart ?salesEnd)
                            (s/in ?fromAirport #{"http://travelplanning.ex/Airport/VNO",
                                               "<http://travelplanning.ex/Airport/KUN>", ;<-
                                               "PLQ"}))) ;todo remove r/to-rdftype

                 :limit 1
                 })

(def fn-filter-result
  (fn [vars]
    (str
      " FILTER ("
      "("
      "?salesStart "
      "<"
      (r/to-rdftype (r/now))
      ")"
      " && "
      "("
      "?salesEnd"
      ">"
      (r/to-rdftype (r/add-days (s/var-val vars :?dateEnd) 1))
      ")"
      " && "
      "("
      "?salesStart"
      "<"
      (r/to-rdftype (s/var-val vars :?dateEnd))
      ")"
      "&&"
      "("
      "?fromAirport"
      " IN "
      "("
      "<http://travelplanning.ex/Airport/KUN>"
      ","
      "<http://travelplanning.ex/Airport/VNO>"
      ")"
      ")"
      ")"
      ))
  )

(def a-query2b {:select
                       '#{?airline ?salesStart ?travelStart ?travelEnd ?connection ?toAirport ?fromAirport}
                :where
                       '((;(var-out vars :campaign)
                           ?campaign
                           rdf:type tc:PromotionalCampaignType
                           t:campaignAirline ?airline
                           t:campaignSalesStartDate ?salesStart
                           t:campaignSalesEndDate ?salesEnd
                           t:campaignOfferTravelStartDate ?travelStart
                           t:campaignOfferTravelEndDate ?travelEnd)
                          (?connection
                            t:ConnectionAirline ?airline
                            t:ConnectionFromAirport ?fromAirport
                            t:ConnectionToAirport ?toAirport)
                          :filter
                          (and
                            (< ?salesStart (r/to-rdftype (r/now)))
                            (> ?salesEnd (r/to-rdftype (r/add-days ?dateEnd 1))))) ;todo remove r/to-rdftype
                :limit 1
                })


(comment defn a-query2b-f
  (fn [vars namespaces-prefixes]
    (s/sparql-query3 {:select
                             '#{?airline ?salesStart ?travelStart ?travelEnd ?connection ?toAirport ?fromAirport}
                      :where
                             ([(var-out vars :campaign)
                                 'rdf:type 'tc:PromotionalCampaignType
                                 't:campaignAirline '?airline
                                 't:campaignSalesStartDate '?salesStart
                                 't:campaignSalesEndDate '?salesEnd
                                 't:campaignOfferTravelStartDate '?travelStart
                                 't:campaignOfferTravelEndDate '?travelEnd]
                               ['?connection
                                't:ConnectionAirline '?airline
                                't:ConnectionFromAirport '?fromAirport
                                't:ConnectionToAirport '?toAirport]
                               :filter
                               (('?salesStart < (r/to-rdftype (r/now)))
                                 :and
                                 ('?salesEnd > (r/to-rdftype (r/add-days (var-val vars :dateEnd) 1))))) ;todo remove r/to-rdftype
                      :limit 1
                      })
    ))


(comment
  ;query1
  (unify vars variables-defs (prefix (namespaces-prefixes)
           (limit
             (project :all-vars
                      (filter
                        (bgp [?campaign
                              rdf:type tc:PromotionalCampaignType
                              t:campaignAirline ?airline
                              t:campaignSalesStartDate ?salesStart
                              t:campaignSalesEndDate ?salesEnd
                              t:campaignOfferTravelStartDate ?travelStart
                              t:campaignOfferTravelEndDate ?travelEnd]
                             [?connection
                              t:ConnectionAirline ?airline
                              t:ConnectionFromAirport ?fromAirport
                              t:ConnectionToAirport ?toAirport]
                             )
                        (and
                          (< ?salesStart (r/to-rdftype (r/now)))
                          (> ?salesEnd (r/to-rdftype (r/add-days ?dateEnd 1)))))) ;dateEnd lest say parameter
             1))))


(comment
  ;query-result that was potentially created from query1
  (fn [vars var-defs namespaces]
    (prefix (get-ns namespaces-prefixes)
       (limit
         (project '#{?airline ?salesStart ?salesEnd ?travelStart ?travelEnd ?connection ?toAirport ?fromAirport}
                  (filter
                    (bgp [(var-out vars variables-defs namespaces-prefixes :campaign)
                          :rdf:type :tc:PromotionalCampaignType
                          :t:campaignAirline '?airline
                          :t:campaignSalesStartDate '?salesStart
                          :t:campaignSalesEndDate '?salesEnd
                          :t:campaignOfferTravelStartDate '?travelStart
                          :t:campaignOfferTravelEndDate '?travelEnd]
                         ['?connection
                          :t:ConnectionAirline '?airline
                          :t:ConnectionFromAirport '?fromAirport
                          :t:ConnectionToAirport '?toAirport
                          ]
                         )
                    (and
                      (< '?salesStart (r/to-rdftype (r/now)))
                      (> '?salesEnd (r/to-rdftype (r/add-days (vars :dateEnd) 1))))))
         1))))


(def string-query-example
  ;query as fn
  (fn [vars]
    (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX t: <http://travelplanning.ex/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    prefix tc:    <http://travelplanning.ex/Campaign/>
    prefix ta:    <http://travelplanning.ex/Airline/>n"

      (format "select ?connection ?fromAirport ?toAirport
                where {
                   ?connection t:ConnectionAirline %s ;
                   t:ConnectionFromAirport ?fromAirport ;
                   t:ConnectionToAirport ?toAirport .
                   }
                  Limit 1" (str "ta:" (vars :airline)))))
  )

(comment
  (r/rdf
    (r/gen-id "http://travelplanning.ex/" "ConnectionUpdate" ?airline ?fromAirport ?toAirport (r/dateTime-to-id (r/now)))
    (t :ConnectionUpdateConnection)
    (t (str "Connection/" ?connection))
    (r/rei
      (t :RangeStart) (r/literal (r/dateTime-to-date ?beginDate) :type (xsd :date))
      (t :RangeEnd) (r/literal (r/dateTime-to-date ?endDate) :type (xsd :date))
      (t :RequestStartedDate) (r/to-rdftype (r/now)))))

(comment
  (tripple ?campaign rdf:type tc:PromotionalCampaignType)
  (triple-pred-list
    ?campaign
      rdf:type tc:PromotionalCampaignType
      t:campaignAirline ?airline
      t:campaignSalesStartDate ?salesStart
      t:campaignSalesEndDate ?salesEnd
      t:campaignOfferTravelStartDate ?travelStart
      t:campaignOfferTravelEndDate ?travelEnd)
  (triple-pred-list
    ?connection
      t:ConnectionAirline ?airline
      t:ConnectionFromAirport ?fromAirport
      t:ConnectionToAirport ?toAirport)
  ()
  )
