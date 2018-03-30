(ns test-project.jena
  (:require [clojure.data.json :as json])
  (:import [org.apache.jena.rdf.model Model ModelFactory ResourceFactory]
          [org.apache.jena.query Query QueryFactory QueryExecutionFactory ResultSetFormatter]
          [java.io ByteArrayInputStream ByteArrayOutputStream] ))


(defn- query-model
  [model query]
  (if (not (nil? query))
    (let* [qex (QueryExecutionFactory/create (QueryFactory/create query) model)]
      (try
        (let* [res (.execSelect qex)
               os (ByteArrayOutputStream. )]
          (ResultSetFormatter/outputAsJSON os res)
          (json/read-str (slurp (ByteArrayInputStream. (.toByteArray os))) :key-fn keyword)
          )
        ;(json/read-str (ResultSetFormatter/outputAsJSON (.execSelect qex)) :key-fn keyword)
        (finally (.close qex))
        ))
    false))

(defn load-and-eval-jena-model
  "if nothing found empty [] returned"
  [data-input query ]
  ;(println data-input)
  (let*
    [m (ModelFactory/createDefaultModel)]
    (.read m (ByteArrayInputStream. (.getBytes data-input)) nil "TTL")
    (((query-model m query) :results) :bindings)
    ;(if (query-model m query)  (apply f data-input)  nil  )
    ))

(comment
  ((j/load-and-eval-jena-model "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \nPREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\nPREFIX t: <http://travelplanning.ex/>\nPREFIX ta: <http://travelplanning.ex/Airport/>\nPREFIX tc: <http://travelplanning.ex/Campaign/>\nPREFIX te: <http://travelplanning.ex/Event/>\nPREFIX et: <http://travelplanning.ex/Transaction/>\nPREFIX tt: <http://travelplanning.ex/TransactionTriple/>\nprefix tair: <http://travelplanning.ex/Airline/>\n\n tc:123456997 rdf:type tc:PromotionalCampaignType.\n  tc:123456999 rdf:type tc:PromotionalCampaignType;\n  t:campaignAirline tair:W6;\n  t:campaignSalesStartDate \"\"\"2017-10-16T00:00:01.390Z\"\"\"^^xsd:dateTime;\n  t:campaignSalesEndDate \"\"\"2017-10-19T00:00:01.390Z\"\"\"^^xsd:dateTime ;\n  t:campaignOfferTravelStartDate \"\"\"2017-11-20T00:00:01.390Z\"\"\"^^xsd:dateTime ;\n  t:campaignOfferTravelEndDate \"\"\"2017-12-20T00:00:01.390Z\"\"\"^^xsd:dateTime." "SELECT ?campaign where {?campaign \n<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>\n    <http://travelplanning.ex/Campaign/PromotionalCampaignType>}") :results))