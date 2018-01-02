(ns test-project.test)



;<http://travelplanning.ex/ConnectionUpdate/W6_VNO_LTN_20170911080715>
(comment added-resource ?c
         rdf:type t:PromotionalCampaign
         t:airline ?a
         t:campaignSalesStartDate ?salesStart
         t:campaignSalesEndDate ?salesEnd
         t:campaignOfferTravelStartDate ?travelStart
         t:campaignOfferTravelEndDate ?travelEnd
         (Filter (and (< t:campaignSalesStartDate r/now)
                      (> t:campaignSalesEndDate r/now))))

(comment
  (r/defns rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  (r/defns t "http://travelplanning.ex/")
  (r/defns xsd "http://www.w3.org/2001/XMLSchema#"))


; cia tipo kad campaing'as prasidejo



(comment defn add-resource
         [?c ?a ?salesStart ?salesEnd  ]                               ;?travelStart ?travelEnd - poto prideti
         (ct/rdf
           (ct/iri ?c) (ct/a) (t :PromotionalCampaign)
           (ct/rei
             (t :airline) (ct/iri ?a)
             (t :campaignSalesStartDate) (ct/literal ?salesStart :type (xsd :dateTime))
             (t :campaignSalesEndDate) (ct/literal ?salesEnd :type (xsd :dateTime))
             )))

(comment defn add-resource
         [?c ?a ?salesStart ?salesEnd  ]                               ;?travelStart ?travelEnd - poto prideti
         (triple-pred-list
           ?c :ct:a t:PromotionalCampaign
             :t:airline ?a
             :t:campaignSalesStartDate ?salesStart
             :t:campaignSalesEndDate ?salesEnd))
(comment
  )