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

(defn exec-action-base [req af]
  (let [rj (-> req jl/context-vars-map-to-json-ld (json/write-str))]
    ;    (println rj)
    (-> (ws/CallWS "http://localhost:3030/Test2" rj {"Content-Type" "application/ld+json; charset=utf-8"})
        ;(println)
        )
    (let [query-params-simplified {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}
          ]
      ;(println query-params-simplified)
      ; execute main function
      ; ideally we should transform result to clojure map, but currently use just string representation
      (let [result-string (af)
            ; result-map (json/read-str result-string :key-fn keyword)
            ]
        (-> (ws/CallWS "http://localhost:3030/Test2" result-string {"Content-Type" "application/ld+json; charset=utf-8"})
            ;(println )
            )
        result-string)
      ;id
      )))


(defn exec-generic-ws-action [req]
  (exec-action-base req (fn [] (ws/GetWS
                           (ctx/var-val req :url)
                           {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}))))

(defn exec-mental
  "executed mental action which is <insert from select> type data manipulation
  r - is sparql update string. Difference with other actions that request doesn't have to be captured"
  [r]
  (println "exec mental" r)
  (ws/CallWS "http://localhost:3030/Test2/update" r {"Content-Type" "application/sparql-update; charset=utf-8"}))

(defn exec-action-fn
  [f ar]
  (let [req {:id   (ctx/time-id "http://travelplanning.ex/Request/")
             :fn   (str f)
             :args (assoc ar :id (ctx/time-id "http://travelplanning.ex/Request/Args") ) }]
    (exec-action-base req #(f ar))))


(defn exec-get-action
  [url namespaces-prefixes params-map]
  (exec-generic-action url namespaces-prefixes #(ws/GetWS url params-map)))

(defmacro add-facts
  "n - namespaces
  p - params
  i - insert triples
  w - where part"
  [n p i w ]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (list :action :mental-action
         ~(s/build-insert n p i w context-v)) )))

(defmacro retact-facts
  "n - namespaces
p - params
d - data, triples to delete
w - where part"
  [n p d]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (list :action :mental-action
             ~(s/build-delete n p d context-v)) )))

