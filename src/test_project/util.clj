(ns test-project.util
  (:use [clojure.pprint]))

(defn print-and-out
  [v]
  (print v)
  v)


(defn eta-keyword [k]
  (keyword (str "@" k)))

(def idk (eta-keyword "id"))
(def contextk (eta-keyword "context"))

(defn println-and-last-out [& params]
  (apply print params)
  (last params))



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

