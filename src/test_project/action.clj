(ns test-project.action
  (:require [test-project.ws :as ws]
            [test-project.rdf :as r]
            [test-project.sparql :as s]
            [test-project.jena :as j]
            [test-project.util :as u]
            [clojure.data.json :as json]))


(defn call-action
  ([url] (call-action url nil))
  ([url body]
    (ws/CallWS url (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" body)
              {"Content-Type" "text/turtle"})))

(defn action
  [action-key & params]
  (list :action action-key params)
  )

(defn exec-generic-action
  [url namespaces-prefixes f]
  (let [request-id (str "http://travelplanning.ex/Request/" (r/dateTime-to-id (r/now)))
        req (r/rdf namespaces-prefixes
                   [(r/iri request-id)
                    :t:URL (r/iri url)
                    :t:Method (r/iri "http://get")])]
    (ws/CallWS
      "http://localhost:3030/Test2" req
      {"Content-Type" "text/turtle; charset=utf-8"})
    (let [result-string (f)
          result-map {
                      (keyword "@id")      request-id
                      (keyword "@context") {:ResponseBody "http://tvavelplanning.ex/Response/Body"}
                      :ResponseBody        (json/read-str result-string :key-fn keyword)}]
      (->
        (json/write-str result-map)
        (j/read-and-output-model "JSON-LD" "TTL")
        (#(ws/CallWS "http://localhost:3030/Test2" % {"Content-Type" "text/turtle; charset=utf-8"})))
      request-id )))



(defn context-vars-map-to-json-ld [m ns]
  (->
    (assoc m :query-params (reduce-kv #(assoc %1 %2 (s/var-full-val-out %3)) {} (m :query-params)))
    ;root element should have :id key
    (assoc u/idk (s/var-full-val-out (m :id)))
    (assoc u/contextk ns)
    ;(json/write-str)
  ; we need extract @context (json-ld)
  ; from variable definition whe can infer @type (json-ld)
  ; we need to get URI for keywords
  ; currently
    )
    )



(defn exec-generic-action2
  [{:keys [id url method query-params]}]
  (let [req {u/idk     id
             :t:URL    url
             :t:Method method
             :t:RequestBody query-params}]
    (ws/CallWS
      "http://localhost:3030/Test2" req
      {"Content-Type" "application/ld+json; charset=utf-8"})
    (let [query-params-simplified (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} query-params)
          result-string (ws/GetWS url query-params-simplified)
          result-map {
                      u/idk id
                      (keyword "@context") {:ResponseBody "http://tvavelplanning.ex/Response/Body"}
                      :ResponseBody        (json/read-str result-string :key-fn keyword)}]
      (->
        (json/write-str result-map)
        (j/read-and-output-model "JSON-LD" "TTL")
        (#(ws/CallWS "http://localhost:3030/Test2" % {"Content-Type" "text/turtle; charset=utf-8"})))
      id )))

()
(defn exec-get-action
  [url namespaces-prefixes params-map]
  (exec-generic-action url namespaces-prefixes #(ws/GetWS url params-map))
)

