(ns test-project.json-ld
  (:require [test-project.util :as u]
            [test-project.sparql :as s]))

(def idk (u/eta-keyword "id"))
(def contextk (u/eta-keyword "context"))
(def vocabk (u/eta-keyword "vocab"))
(def valuek (u/eta-keyword "value"))
(def typek (u/eta-keyword "type"))

(def http-methods {:get {:type :uri, :value "GET" :prefix-ns "http://www.w3.org/2011/http-methods#"}})


(defn val-out
  "return value ready to converting to json-ld"
  [v]
  ;(println (and (map? v) (:type v) (:value v)))
  (if (and (map? v) (:type v) (:value v))
    ; complex type to output
    (case (v :type)
       :uri {idk (str (v :prefix-ns) (v :value))}
       :long {valuek (v :value)}
       :date {valuek (.format (v :value) (java.time.format.DateTimeFormatter/ISO_DATE)),
              typek  "http://www.w3.org/2001/XMLSchema#date"}
       :dateTime {valuek (.format (v :value) (java.time.format.DateTimeFormatter/ISO_INSTANT)),
                  typek  "http://www.w3.org/2001/XMLSchema#dateTime"}
       :literal (case (v :datatype)
                  "http://www.w3.org/2001/XMLSchema#dateTime" {valuek (.format (v :value) (java.time.format.DateTimeFormatter/ISO_INSTANT)),
                                                               typek  "http://www.w3.org/2001/XMLSchema#dateTime"}
                  "http://www.w3.org/2001/XMLSchema#date" {valuek (.format (v :value) (java.time.format.DateTimeFormatter/ISO_DATE)),
                                                           typek  "http://www.w3.org/2001/XMLSchema#date"}
                  {valuek (v :value) typek (v :datatype)}
                  )
       (v :value))
    (cond
      (= (type v) java.lang.String) (val-out {:value v, :type :string})
      (= (type v) java.time.ZonedDateTime) (val-out {:value v, :type :dateTime})
      (= (type v) java.lang.Double) (val-out {:value v, :type :double})
      (= (type v) java.lang.Long) (val-out {:value v, :type :long}))))


(defn cr-var-out
  "complex recursive variable output
  takes map, key and function
  if it is a context value outputs, if complex applies f (idea - to apply recursively)
  if simlpe v"
  [m k f]
  (let [v (k m)]
    ; if it is context-var
    (if (map? v)
      (if (and (:type v) (:value v))
        (val-out v)
        (f v (keys v)))
      (val-out v))))




(defn context-vars-map-to-json-ld
  "maps context to json-ld ready map
  m - map
  ks - keys list"
  ([m] (context-vars-map-to-json-ld m (keys m)))
  ([m ks]
   (if
     (empty? m)
     m
     (->
      ; if it
      ;(assoc m :query-params (reduce-kv #(assoc %1 %2 (s/var-full-val-out %3)) {} (m :query-params))
      (reduce #(assoc %1 %2 (cr-var-out %1 %2 context-vars-map-to-json-ld)) m ks)
      ;root element should have :id key
      (assoc u/idk (s/var-full-val-out (m :id)))
      (dissoc :id)
      (assoc u/contextk (assoc {} u/vocabk (-> m :id :prefix-ns)))
      ;(u/print-and-out)
      ; we need extract @context (json-ld)
      ; from variable definition whe can infer @type (json-ld)
      ; we need to get URI for keywords. URI is parents prefix + keywordvalue, but can add to context main
      ))))
