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


(defn add-action
  [action-key & params]
  (list :action action-key params))

(defmacro action [key & params]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (add-action ~key ~@(ctx/replace-2var-lookup context-v params)))))




(defn exec-action-base [req af]
  (let [rj (-> req jl/context-vars-map-to-json-ld (json/write-str))]
    ;(println "exec-action-base:\n" rj)
        (-> (ws/CallWS "http://localhost:3030/Test2" rj {"Content-Type" "application/ld+json; charset=utf-8"}))
        (let [query-params-simplified {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}
          ]
      ;(println query-params-simplified)
      ; execute main function
      ; ideally we should transform result to clojure map, but currently use just string representation
        (let [result-string (af)
              ; result-map (json/read-str result-string :key-fn keyword)
              ]
          (if (not= result-string :error-action)
            (-> (ws/CallWS "http://localhost:3030/Test2" result-string {"Content-Type" "application/ld+json; charset=utf-8"})
                ;(println )
                ))
          result-string
          )
        ;id
        )))


(defn exec-generic-ws-action [req]
  (exec-action-base req (fn [] (ws/GetWS
                           (ctx/var-val req :url)
                           {:query-params (reduce-kv #(assoc %1 %2 (s/var-short-val-out %3)) {} (req :query-params))}))))

(defn exec-mental
  "executed mental action which is <detele insert from select> type data manipulation
  r - is sparql update string. Difference with other actions that request doesn't have to be captured"
  [r]
  (println "exec mental" r)
  (ws/CallWS "http://localhost:3030/Test2/update" r {"Content-Type" "application/sparql-update; charset=utf-8"}))

(defn exec-action-fn
  "executed actions based on some clojure function not ws call"
  [f ar]
  (let [req {:id   (ctx/time-id "http://travelplanning.ex/Request/")
             :fn   (str f)
             :args (assoc ar :id (ctx/time-id "http://travelplanning.ex/Request/Args") ) }]
    (exec-action-base req #(f ar))))


(defmacro add-facts
  "n - namespaces
  p - params
  i - insert triples
  w - where part"
  [n p i w ]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (list :action
             :mental-action
             (list (str
                (s/namespaces-prefixes-map-to-spaqrl ~n)
                " "
                (u/join-r " "
                          ~(s/build-insert p i w context-v))))))))

(defmacro update-facts
"n - namespaces
p - params
form - data which contains :insert (insert-form) :delete (delete-form) :where {where-form}"
  [n p form]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (list :action
             :mental-action
             (list (u/join-r " "
                        [(s/namespaces-prefixes-map-to-spaqrl ~n)
                         ~@(s/build-update p form context-v)]))))))



