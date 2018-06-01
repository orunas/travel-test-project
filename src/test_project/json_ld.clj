(ns test-project.json-ld
  (:require [test-project.util :as u]))

(def idk (u/eta-keyword "id"))
(def contextk (u/eta-keyword "context"))
(def vocabk (u/eta-keyword "vocab"))
(def valuek (u/eta-keyword "value"))
(def typek (u/eta-keyword "type"))

(def http-methods {:get {:type :uri, :value "GET" :prefix-ns "http://www.w3.org/2011/http-methods#"}})

(defn id
  "get id for map
  m - map
  p - parent key"
  [m p]
  (if-let [v (m :id)]
    v

    ))

(defn val-out
  "return value ready to converting to json-ld"
  [var-ctx]
  (case (var-ctx :type)
    :uri { idk (str (var-ctx :prefix-ns) (var-ctx :value))}
    :date {valuek (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_DATE)),
           typek "http://www.w3.org/2001/XMLSchema#date"}
    :dateTime {valuek  (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_INSTANT)),
               typek "http://www.w3.org/2001/XMLSchema#dateTime" }
    :literal (case (var-ctx :datatype)
               "http://www.w3.org/2001/XMLSchema#dateTime" { valuek (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_INSTANT)),
                                                            typek "http://www.w3.org/2001/XMLSchema#dateTime"}
               "http://www.w3.org/2001/XMLSchema#date" {valuek (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_DATE)),
                                                        typek "http://www.w3.org/2001/XMLSchema#date" }
               {valuek (var-ctx :value) typek (var-ctx :datatype)}
               )
    (var-ctx :value)))
