(ns test-project.action
  (:require [test-project.ws :as ws]
            [test-project.rdf :as r]
            [test-project.sparql :as s]
            [test-project.context :as ctx]
            [test-project.jena :as j]
            [test-project.util :as u]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [test-project.json-ld :as jl]))


(defn call-action
  ([url] (call-action url nil))
  ([url body]
    (ws/CallWS url (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" body)
              {"Content-Type" "text/turtle"})))


(defn action
  [action-key & params]
  (list :action action-key params))


(defn exec-generic-action
  [url namespaces-prefixes f]
  (let [request-id (str "http://travelplanning.ex/Request/" (u/dateTime-to-id (r/now)))
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

(defn exec-action-base [req f]
  (let [rj (-> req jl/context-vars-map-to-json-ld (json/write-str))]
    (println rj)
    (println (ws/CallWS "http://localhost:3030/Test2" rj {"Content-Type" "application/ld+json; charset=utf-8"}))
    (let [query-params-simplified {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}
          ]
      (println query-params-simplified)
      (let [result-string f
            result-map (json/read-str result-string :key-fn keyword)]
        (-> (ws/CallWS "http://localhost:3030/Test2" result-string {"Content-Type" "application/ld+json; charset=utf-8"})
            (println ))
        result-map)
      ;id
      )))

(defn exec-generic-ws-action [req]
  (exec-action-base req (fn [] (ws/GetWS
                           (ctx/var-val req :url)
                           {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}))))



(comment defn exec-action-get-airport-data [a]
  (let [req {:id (ctx/time-id "http://travelplanning.ex/Request/")
             :url "local://exec-action-get-airport-data"
             :query-params {:airport a}}]
    (exec-action-base req #(add-airport-data)))
  )

(defn exec-get-action
  [url namespaces-prefixes params-map]
  (exec-generic-action url namespaces-prefixes #(ws/GetWS url params-map)))

