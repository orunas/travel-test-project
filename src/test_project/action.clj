(ns test-project.action
  (:require [test-project.ws :as ws]))


(defn call-action
  ([url] (call-action url nil))
  ([url body]
    (ws/CallWS url (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" body)
              {"Content-Type" "text/turtle"})))

(defn action
  [action-key & params]
  (list :action action-key params)
  )


