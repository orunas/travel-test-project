(ns test-project.res  (:require [test-project.rdf :as ct]))

(ct/defns t "http://travelplanning.ex/")
(ct/defns xsd "http://www.w3.org/2001/XMLSchema#")
(defn add-resource
  [?c ?a ?salesStart ?salesEnd  ]                               ;?travelStart ?travelEnd - poto prideti
  (ct/rdf
    (ct/iri ?c) (ct/a) (t :PromotionalCampaign)
    (ct/rei
      (t :airline) (ct/iri ?a)
      (t :campaignSalesStartDate) (ct/literal ?salesStart :type (xsd :dateTime))
      (t :campaignSalesEndDate) (ct/literal ?salesEnd :type (xsd :dateTime))
      )))

; example call (res/add-resource "http://travelplanning.ex/12563" "http://travelplanning.ex/Airline/W6" "2017-08-29T17:40:39Z" "2017-08-31T17:40:39Z" )