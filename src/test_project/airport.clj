(ns test-project.airport
  (:require [clojure.data.json :as json]
            [test-project.ws :as ws]))

(defn get-first-data-from-dbpedia [iata]
  (let [s (str "select distinct ?airport ?iata ?lat ?long  where
  {?airport a <http://dbpedia.org/ontology/Airport>.
  ?airport dbo:iataLocationIdentifier ?iata.
    ?airport geo:lat ?lat.\n?airport geo:long ?long.\nFILTER (?iata = \"" iata "\")\n} LIMIT 100")]
    (-> (ws/Post2WS "http://dbpedia.org/sparql" {:form-params {:query s :format "application/sparql-results+json"}})
        (json/read-str :key-fn keyword)
        :results
        :bindings
        first)))
(defn get-from-local [iata]
  (-> (ws/GetWS (str "http://localhost:8087/airport/" iata) {:as :text})
      (json/read-str :key-fn keyword)))

(defn get-data-from-geonames [long lat]
  (let [q {:query-params {:username "orunas" :lat lat :lng long}}]
    (-> (ws/GetWS "http://api.geonames.org/timezoneJSON" q)
        (json/read-str :key-fn keyword))))

(defn get-airport-timezone [iata]
  (if-let [first-binding (get-first-data-from-dbpedia iata)]
    (get-data-from-geonames (-> first-binding :long :value)  (-> first-binding :lat :value) )
    (do
      (println "could not get anything from dbpedia for airport: " iata)
      (if-let [first-binding (get-from-local iata)]
       (get-data-from-geonames (first-binding :longitude) (first-binding :latitude))
       (println "error - not data for " iata)))))
