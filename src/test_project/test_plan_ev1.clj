(ns test-project.test-plan-ev1
  (:require
    [test-project.action :as a]
    [test-project.ea :as e])
  )


(def namespaces-prefixes
  {
   :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   :xsd "http://www.w3.org/2001/XMLSchema#"
   :rd "http://ev.ex/Road/"
   :st "http://ev.ex/Station/"
   :treq "http://ev.ex/Request/"
   :tresp "http://ev.ex/Response/"
   })


(def actions
  {
   :call-action-generic         #(test-project.action/call-action %1 %2)
   :call-action-ws-generic        #(test-project.action/exec-generic-ws-action %)
   :exec-action-fn            #(test-project.action/exec-action-fn %1 %2)
   :call-action-test            (fn [var1] (print "doing something"))
   :mental-action #(test-project.action/exec-mental %)
   }
  )

(def loc-methods-lib (atom {}) )

(e/def-method check-station-form [?r]
                :task  (:check-station )
                :namespaces namespaces-prefixes :actions actions :methods loc-methods-lib
                :precondition ((?r treq:RequestedStation ?station )
                                (?station st:ID ?id))
                :body [(fn [_] (println "connection data up to date!"))])