(ns test-project.test-plan2
  (:require [clojure.test :refer :all]
            [test-project.core :as c]
            [test-project.sparql :as s ]
            [test-project.test-plan :as t :refer :all]
            [test-project.rdf :as r]
            [test-project.ea :as e]))

(comment                                                    ;examples for cure
  :cue (task :get-home ?beginDate ?endDate)
  :cue (event :insert-bgp ?campaign :rdf:type :tc:PromotionalCampaignType)
  :cue (event :insert-subject-uri :tc ?campaign)

  :task (get-home ?beginDate ?endDate)
  :event (+bgp ?campaign :rdf:type :tc:PromotionalCampaignType)
  :event (+subject-uri :tc ?campaign)
  )

(e/defplan
  update-data-on-campaign3
  []
  ;for event type plans parameters are in event desciption
  ; +bgp does nothing. All events are additions of some information
  :event [?campaign :rdf:type :tc:PromotionalCampaignType]
  :namespaces t/namespaces-prefixes
  :precondition ((?campaign
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
                  (s/f-and (s/f< ?salesStart (r/now))
                           ; (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                           ;(s/f< ?salesStart ?dateEnd)
                           (s/f< ?salesStart ?salesEnd)
                           (s/f-in
                             ?fromAirport
                             #{"<http://travelplanning.ex/Airport/KUN>"
                               "<http://travelplanning.ex/Airport/VNO>"
                               "<http://travelplanning.ex/Airport/PLQ>"})))
  :body
    [true
     [(fn [vars]
         (test-project.core/call-action
           "http://localhost:8080/flightService/webapi/W6"
           (r/rdf
             [(r/gen-id "http://travelplanning.ex/" "ConnectionUpdate"
                        (s/var-val vars :?airline)
                        (s/var-val vars :?fromAirport)
                        (s/var-val vars :?toAirport)
                        (r/dateTime-to-id (r/now)))
              :t:ConnectionUpdateConnection (str "t:Connection/" (s/var-val vars :?connection))
              :t:RangeStart (r/literal (r/dateTime-to-date (s/var-val vars :?beginDate)) :type "<http://www.w3.org/2001/XMLSchema#date>")
              :t:RangeEnd (r/literal (r/dateTime-to-date (s/var-val vars :?endDate)) :type "<http://www.w3.org/2001/XMLSchema#date>")
              :t:RequestStartedDate (r/to-rdftype (r/now))])))
        (fn [vars]
          (test-project.core/add-task
            :update-data-task
            (s/var-val vars :?airline)
            (s/var-val vars :?endDate)
            (r/add-days (s/var-val vars :?endDate) 7)
            (s/var-val vars :?salesStart)
            (s/var-val vars :?salesEnd)))]])