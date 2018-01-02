(ns test-project.sparql-test
  (:require [clojure.test :refer :all]
            [test-project.core :as c ]
            [test-project.sparql-o :as so]
            [test-project.sparql :as s]
            [test-project.test-plan :as t]
            [test-project.rdf :as r]))


(deftest tripleSameSubjTest                                                     ;kolkas
  (is
    (= "<http://travelplanning.ex/Campaign/1233>t:hasHome?c.<http://travelplanning.ex/Campaign/1233>t:kitas?m."
       (clojure.string/replace
         ((so/tripleSameSubj ?campaign t:hasHome ?c t:kitas ?m)
           {:campaign "1233"}
           {:campaign {:type "uri" :prefix "http://travelplanning.ex/Campaign/"}})
         #"\s"
         ""))))

(deftest var-out-test
  (is
    (=
      (s/var-out {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}} :?campaign)
      "<http://travelplanning.ex/Campaign/1234>")))

(deftest var-val-test
  (is
    (=
      (s/var-val  {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}} :?campaign )
      "1234")))

(comment
  (s/replace-var-to-varskeymap-lookup {:params '#{?campaign} :vars-name 'vars1} '?campaign)
  '(test-project.sparql-o/var-val vars1 :?campaign))

(comment "should be nil"
         (s/var-val {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}} :?dateEnd))

(comment
  ((s/build-where-query ((?campaign
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
                                     (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                                     (s/f< ?salesStart ?dateEnd)
                                     (s/f< ?salesStart ?salesEnd)
                                     (s/f-in
                                       ?fromAirport
                                       #{"<http://travelplanning.ex/Airport/KUN>"
                                         "<http://travelplanning.ex/Airport/VNO>"
                                         "<http://travelplanning.ex/Airport/PLQ>"})))
                        #{?campaign ?dateEnd}) {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}}))

(comment
  (macroexpand '(s/buildfilterfn {:params #{?dateEnd ?campaign}, :vars-name vars}
                               (s/f-and (s/f< ?salesStart (r/now))
                                        (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                                        (s/f< ?salesStart ?dateEnd)
                                        (s/f< ?salesStart ?salesEnd)
                                        (s/f-in
                                          ?fromAirport
                                          #{"<http://travelplanning.ex/Airport/KUN>"
                                            "<http://travelplanning.ex/Airport/VNO>"
                                            "<http://travelplanning.ex/Airport/PLQ>"})))))


(comment
  (macroexpand
    '(s/buildfilterfn
       {:params #{?dateEnd ?campaign}, :vars-name vars}
       (s/f-in
         ?fromAirport
         #{"<http://travelplanning.ex/Airport/KUN>"
           "<http://travelplanning.ex/Airport/VNO>"
           "<http://travelplanning.ex/Airport/PLQ>"}))))


(comment
  {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}
   :?dateEnd {:type :literal :value (r/now)}})

(comment
  (print
    ((s/build-pre-query
       t/namespaces-prefixes
       #{?campaign ?dateEnd}
       ((?campaign
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
                  (s/f> ?salesEnd (r/add-days ?dateEnd 1))
                  (s/f< ?salesStart ?dateEnd)
                  (s/f< ?salesStart ?salesEnd)
                  (s/f-in
                    ?fromAirport
                    #{"<http://travelplanning.ex/Airport/KUN>"
                      "<http://travelplanning.ex/Airport/VNO>"
                      "<http://travelplanning.ex/Airport/PLQ>"}))))
      {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}
       :?dateEnd  {:type :literal :value (r/now)}})))

