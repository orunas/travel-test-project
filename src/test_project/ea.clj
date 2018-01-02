(ns test-project.ea
  (:require [test-project.rdf :as r] [test-project.core :as c] [test-project.sparql :as s] :reload)
  )

(def plans (atom {}) )

(defmacro defplan
  [name parameters & {:keys [event namespaces precondition body]}]
  (comment `(def ~name {:event        (s/build-event ~namespaces (quote ~event))
                :precondition (s/build-pre-query ~namespaces ~(set parameters) ~precondition)
                :body         ~body}))
  `(swap! e/plans  assoc  (keyword (quote ~name))
         {:event        (s/build-event ~namespaces (quote ~event))
          :precondition (s/build-pre-query ~namespaces ~(set parameters) ~precondition)
          :body         ~body}))


