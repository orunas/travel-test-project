(ns test-project.context
  (:require [test-project.util :as u]
            [test-project.rdf :as r]))
;namespace for managing context variables


(defn time-id [ns & p]
  {:type :uri :value (str (apply str p) (u/dateTime-to-id (r/now)) ) :prefix-ns ns}
  )

;(def-multi var-val)
(defn var-val
  ([v]
   (if (map? v)
     (v :value)
     v))
  ([context var]
   (var-val (context var))))


(defn datetime-var [val]
  {:type :dateTime
   :value val})


