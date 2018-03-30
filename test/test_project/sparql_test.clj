(ns test-project.sparql-test
  (:require [clojure.test :refer :all]
            [test-project.sparql :as s]
            [test-project.rdf :as r]))

(def namespaces-prefixes
  {
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :t "http://travelplanning.ex/"
   :xsd "http://www.w3.org/2001/XMLSchema#"
   :tc "http://travelplanning.ex/Campaign/"
   :ta "http://travelplanning.ex/Airline/"
   :tcn "http://travelplanning.ex/Connection/"
   :tcu "http://travelplanning.ex/ConnectionUpdate/"})

(deftest datetime-to-date-test
  (is
    (=
      {:type :literal, :datatype "http://www.w3.org/2001/XMLSchema#time", :value (java.time.ZonedDateTime/parse "2018-01-19T00:00:01.390Z")}
      (s/datetime-type-to-date {:?salesEnd {:type :literal, :datatype "http://www.w3.org/2001/XMLSchema#dateTime", :value (java.time.ZonedDateTime/parse "2018-01-19T00:00:01.390Z")}} :?salesEnd))))

(deftest var-out-test
  (is
    (=
      (s/var-out {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}} :?campaign)
      "<http://travelplanning.ex/Campaign/1234>")))

(deftest ordering-part-test
  (is
    (=
      " DESC (?campaign)  ASC (?namas)  ASC (?petras) "
      (s/parse-ordering-part '(:desc ?campaign :asc ?namas ?petras)))
    (=
      "ORDERBY  DESC (?createdTime)  ASC (?airline)  ASC (?connection) "
      (s/buid-order-by-part '((?connectionData t:CreatedDateTime ?createdTime
                                               tcd:Connection ?connection)
                               (?connection t:ConnectionAirline ?airline)
                               (:filter (f/< ?airline ?connection))
                               (:order-by (:desc ?createdTime ?airline :asc ?connection)))))))

(deftest var-val-test
  (is
    (=
      (s/var-val  {:?campaign {:type :uri :prefix-ns "http://travelplanning.ex/Campaign/" :value "1234"}} :?campaign )
      "1234"))
  (is
    (=
      "\"\"\"2017-10-10T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime>"
      (s/var-out {:?dateStart {:type     :literal,
                               :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                               :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")}}
                 :?dateStart)))
  (is
    (=
      "\"\"\"2017-10-10Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#date>"
      (s/var-out {:?dateStart {:type     :literal,
                               :datatype "http://www.w3.org/2001/XMLSchema#date",
                               :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")}}
                 :?dateStart))))


(deftest test-build-filter
  (is
    (=
      "(\"\"\"2017-10-10T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime> < ?createdTime)"
      (eval (second (s/buildfilter {:params #{} :vars-name 'vars} '(s/f> ?createdTime (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z") )))))))



(deftest test-build-pre-query
  (is
    (=
      (clojure.string/replace
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>   PREFIX t: <http://travelplanning.ex/>
 PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>   PREFIX tc: <http://travelplanning.ex/Campaign/>
 PREFIX ta: <http://travelplanning.ex/Airline/>  PREFIX tcn: <http://travelplanning.ex/Connection/>
 PREFIX tcu: <http://travelplanning.ex/ConnectionUpdate/>
 ASK WHERE {
 ?connection t:ConnectionAirline <http://travelplanning.ex/Airline/W6>. ?connection t:CreatedDateTime ?createdTime.
   FILTER (\"\"\"2017-10-10T00:00:01.390Z\"\"\"^^<http://www.w3.org/2001/XMLSchema#dateTime> < ?createdTime)}"
        #"\s+" "")
      (clojure.string/replace
        (((test-project.sparql/build-precondition
            namespaces-prefixes
            #{?airline}
            (:not-exists (?connection t:ConnectionAirline ?airline t:CreatedDateTime ?createdTime)
              (:filter
                (s/f> ?createdTime (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z"))))) :not-exists)
          {:?airline {:type :uri, :value "W6", :prefix-ns "http://travelplanning.ex/Airline/"}})
        #"\s+" ""))))

(comment deftest tripleSameSubjTest                                                     ;kolkas
         (is
           (= "<http://travelplanning.ex/Campaign/1233>t:hasHome?c.<http://travelplanning.ex/Campaign/1233>t:kitas?m."
              (clojure.string/replace
                ((s/tripleSameSubj ?campaign t:hasHome ?c t:kitas ?m)
                  {:campaign "1233"}
                  {:campaign {:type "uri" :prefix "http://travelplanning.ex/Campaign/"}})
                #"\s"
                ""))))

(deftest
  test-append-sparql-result-to-context
  (is (=
        {:?campaign   {:type :uri, :value "123456999", :prefix-ns "http://travelplanning.ex/Campaign/"},
         :?salesStart {:type     :literal,
                       :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                       :value    (java.time.ZonedDateTime/parse "2017-10-16T00:00:01.390Z")}}
        (s/append-sparql-result-to-context
          {:campaign   {:type "uri", :value "http://travelplanning.ex/Campaign/123456999"},
           :salesStart {:type     "literal",
                        :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                        :value    "2017-10-16T00:00:01.390Z"}}
          {}))))


(deftest
  test-sparql-var-binding-to-eavariable
  (is
    (=
      {:type     :literal,
       :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
       :value    (java.time.ZonedDateTime/parse "2017-10-10T00:00:01.390Z")}

      (s/sparql-var-binding-to-eavariable
        {:type     "literal",
         :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
         :value    "2017-10-10T00:00:01.39Z"}))))




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

