(ns test-project.context
  (:require [test-project.util :as u]
            [test-project.rdf :as r])
  (:use [clojure.walk]))
;namespace for managing context variables

(defn variable?
  [v]
  (= (first (str v)) \?))

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




(defn replace-2var-lookup
  "recursivelly searches statements and looks for variables and replaces them to vars keymap lookup.
  If variable check function returns true it is replaced by key lookup
  vn - context variable name
  form - parsed data"
  [vn form]
  (postwalk #(if (r/variable? %) (list vn (keyword %) ) % ) form))