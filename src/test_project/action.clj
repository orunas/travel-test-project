(ns test-project.action
  (:require [test-project.ws :as ws]
            [test-project.rdf :as r]
            [test-project.jena :as j]
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

(defn exec-generic-action2
  [url namespaces-prefixes f]
  (let [request-id (str "http://travelplanning.ex/Request/" (r/dateTime-to-id (r/now)))
        req (r/rdf namespaces-prefixes
                   [(r/iri request-id)
                    :t:URL (r/iri url)
                    :t:Method (r/iri "http://post")])]
    (ws/CallWS
      "http://localhost:3030/Test2" req
      {"Content-Type" "application/ld+json; charset=utf-8"})
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


(defn exec-get-action
  [url namespaces-prefixes params-map]
  (exec-generic-action url namespaces-prefixes #(ws/GetWS url params-map))
)

