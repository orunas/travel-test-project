(ns test-project.receive
  (:require [test-project.sparql :as s]
            [test-project.util :as u]
            [test-project.ws :as ws]
            [clojure.data.json :as json]
            [test-project.jena :as j]))
;namespace for receiving and handling events, task. It means checking events, finding candidate instances and filtering


(defn get-events
  "get events. Returns list of events where each event is string with text/turtle formating"
  ([] (get-events []))
  ([o]
   (let [res (slurp (ws/CallWS
                      "http://localhost:8087/queue/events/peek"
                      nil
                      {"Content-Type" "text/turtle" "Accept" "text/turtle ; charset=utf-8"}))]
     (if (clojure.string/blank? res)
       o
       (recur (conj o res))))))

(defn find-relevant-methods-for-event
  "Finds relevant method for particular event
  method candidate is represented by context - variables bound to values.
  Returns list (ordering is not important at this moment) of relevant methods for provided event."
  ([method-list event] (find-relevant-methods-for-event method-list event ()))
  ([method-list event relevant]
   (if-let [mthd (first method-list)]
     (if (mthd :event)
       ;cd is vector of maps
       (let [cd (j/load-and-eval-jena-model event ((mthd :event) :select))]
         (recur
           (rest method-list)
           event
           (if (empty? cd)
             relevant
             ; add to result lit
             (conj
               relevant
               ;since cd is vector we need to apply to everyone.
               (map #(s/append-sparql-result-to-context % {:method (mthd :name)}) cd)
               ))))
       (recur (rest method-list) event relevant))
     ;if rest doesn't finds means we are done, return results (relavant methods found)
     relevant)))

(defn find-all-relevant-methods-for-all-events
  "Finds all methods, that are relevant for events.
  Each event can have more that one method relevant.
  Arguments:
   methods which is list of methods (each method is a map),
   events which is a vector of event data in string with turtle formatting.
  Returns - vector contexts. It is a vector of context lists.
   Each context is map of variables bound with additional key for method :method (value is method name as keyword)
    Variables are bound by evaluating :event part in each method.
   Returned vector size same as events (parameter).
   For example first event has corresponding first item in result vector. If result item is list - is correspond to each method relevant to event.
  If method is relevant doesn't mean that it will be applicable.
  If no methods relevant for event it will have nil in result vector at same index.
  "
  [method-list events]
  (mapv #(flatten (find-relevant-methods-for-event method-list %)) events))

(defn precondition-ws-call
  [context precondition]
  ;(println "sending pre query:\n" precondition )            ;"\n context " context
  (json/read-str
    (ws/CallWS
      "http://localhost:3030/Test2"                         ;"http://localhost:3030/Test2"
      precondition
      {"Content-Type" "application/sparql-query" "Accept" "application/sparql-results+json; charset=utf-8"})
    :key-fn keyword))

(defn add-precondition-query-results-to-context
  "get current variable binding and adds fresh variables received from precondition query
  In reality there may be more that on candidate, however we filter to first.
  Second param is just binding"
  [context b]
  (if (not (empty? b))
    ; Takes only first
    (let [k (first b)]
      ;here we take each variable from result (binding) pass through sparql-var-binding-to-eavariable to get ea ready
      ; (that could be use in our system) and also add ? symbol to variable name (which is keyword). Everything is added
      ; to existing binding with assoc.
      (reduce
        #(assoc %1 (keyword (str \? (name %2))) (s/sparql-var-binding-to-eavariable (k %2)))
        context
        (keys k)))))

(defn eval-precondition
  [context a-method]
  (let [precond (a-method :precondition)]
    (cond
      (nil? precond) context                                ; if nil then precond is true
      (precond :query) (->>
                          (((precondition-ws-call
                              context
                              ; precondition is function that has vars argument which method item (bound with variables)
                              ((precond :query) context))
                             :results) :bindings)
                          (add-precondition-query-results-to-context context))
      (precond :not-exists) (let [res ((precondition-ws-call context ((precond :not-exists) context)) :boolean)]
                              (if (false? res) context))                     ; if we returned - false in booleand keyword then it is ok
      :else context)))

(defn filter-applicable-methods-on-preconditions-for-event
  "gets list of relevant methods for event and evaluates preconditions
  "
  [a-method-lib lst-releavant-mths]
  (->> (map
         #(if ((a-method-lib (% :method)) :precondition) (eval-precondition % (a-method-lib (% :method))) %)
         lst-releavant-mths)
       ; we can remove nil. These appear where relevant method after evaluation of precondition is not applicable
       (remove nil?))
  ;(map #(methods (% :method))  lst-releavant-mths)
  )



(defn filter-applicable-methods-on-preconditions
  "we provide vector with lists of relevant methods for each event
  Result is vector with method bindings (not list of methods)"
  [a-method-lib candidate-select-fn relevant-methods]
  (->>
    (mapv #(filter-applicable-methods-on-preconditions-for-event a-method-lib %) relevant-methods)
    ; one event might have several applicable methods. We need to select on
    ; here it just first
    ;(u/println-and-last-out "all applicable methods:")
    (mapv candidate-select-fn)
    ;(u/println-and-last-out "applicable methods after filter:")
    )

  ;(remove nil?) what to do with events that are unaddressed (not applicable methods). They should have emty list ()
  ; in corresponding vector item.
  )

(defn filter-and-remove-unhandled-events
  [events applicable-mthds-contex]
  (remove
    #(let [r (nil? %)
           i (.indexOf applicable-mthds-contex %)]
       ;(print i)
       (if r (println "undandled event. Index" i ))         ;" Data:" (nth events i)
       r)
    applicable-mthds-contex))
