(ns test-project.jena-test
  (:require [clojure.test :refer :all]
            [test-project.jena :as j]))

(deftest test-query-model
  (is
    (=
      [{:campaign {:type "uri", :value "http://travelplanning.ex/Campaign/123456999"},
        :salesEndDate {:type "literal",
                       :datatype "http://www.w3.org/2001/XMLSchema#dateTime",
                       :value "2017-10-19T00:00:01.390Z"}}]
      (j/load-and-eval-jena-model
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX t: <http://travelplanning.ex/>
        PREFIX ta: <http://travelplanning.ex/Airport/>
        PREFIX tc: <http://travelplanning.ex/Campaign/>
        prefix tair: <http://travelplanning.ex/Airline/>
        tc:123456999 rdf:type tc:PromotionalCampaignType;
        t:campaignAirline tair:W6;
        t:campaignSalesStartDate \"\"\"2017-10-16T00:00:01.390Z\"\"\"^^xsd:dateTime;
        t:campaignSalesEndDate \"\"\"2017-10-19T00:00:01.390Z\"\"\"^^xsd:dateTime ;
        t:campaignOfferTravelStartDate \"\"\"2017-11-20T00:00:01.390Z\"\"\"^^xsd:dateTime ;
        t:campaignOfferTravelEndDate \"\"\"2017-12-20T00:00:01.390Z\"\"\"^^xsd:dateTime."
        "SELECT ?campaign ?salesEndDate
        WHERE { ?campaign <http://travelplanning.ex/campaignSalesEndDate>  ?salesEndDate .}"))))
