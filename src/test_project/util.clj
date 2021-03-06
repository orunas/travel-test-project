(ns test-project.util
  (:use [clojure.pprint])
  (:require [clojure.string :as string]))

(defn print-and-out
  [v]
  (print v)
  v)


(defn capitalize-first [s]
  (str (string/upper-case (first s)) (subs s 1)))

(defn eta-keyword [k]
  (keyword (str "@" k)))

(def idk (eta-keyword "id"))
(def contextk (eta-keyword "context"))
(def vocabk (eta-keyword "vocab"))
(def valuek (eta-keyword "value"))
(def typek (eta-keyword "type"))

(defn println-and-last-out [& params]
  (apply print params)
  (last params))


(defn join-r
  "recursively joins collection elements"
  [sep c]
  (if (coll? c)
    (clojure.string/join sep (map #(join-r sep %) c))
    c))

(defn pprintln-and-out
  ([text f data]
   (println text)
   (pprint (f data))
    data)

  ([text data]
   (println text)
   (pprint data)
   data))


(defn before
  [date1 date2]
  (> (.getSeconds (java.time.Duration/between date1 date2)) 0))

(defn date-diff-in-seconds
  [date1 date2]
  (.getSeconds (java.time.Duration/between date1 date2)))


(defn filter-out-whitespaces [s]
  (clojure.string/replace s #"\s+" ""))

(defn dateTime-to-id
  [v]
  (.format v (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss")))

