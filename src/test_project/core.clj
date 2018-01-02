

(ns test-project.core
  (:require [org.httpkit.client :as http]
            [clj-sparql.core]
            [clojure.data.json :as json]
            [test-project.rdf :as r]
            [clojure.string :as string]
            [test-project.match :as u]
            )
  )

(declare ^:dynamic *vars*)

(defn print-and-out
  [v]
  (print v)
  v)

(defn print-and-out2
  [val-to-print out-value]
  (print val-to-print)
  out-value
  )


(defn variable?
  [v]
  (= \? (first (str v))))

(defn var-name
  [v]
  (if (variable? v)
    (subs (str v) 1)
    nil)
  )

(defn var-out
  [context var-defs var-sym]
  (let [k (keyword var-sym)
        {t :type} (var-defs k)]
    (case t
      "uri" (str "<" ((var-defs k) :prefix) (context k) "> " )
      "literal" (r/to-rdftype (context k))                                            ;(str "\"" (context k) "\"^^" ((var-defs k) :datatype)  " " )
      )
    )
  )

(defn var-in
  [v var-defs var-sym]
  (let [k (keyword var-sym)
        the-var (var-defs k)
        {t :type} the-var]
    ;(println v var-sym k t )
    ;(println var-defs)
    (case t
      "uri" (string/replace v ((var-defs k) :prefix) "" )
      "literal" (case (the-var :datatype)
                  "http://www.w3.org/2001/XMLSchema#dateTime" (java.time.ZonedDateTime/parse v)
                  v)
      v)))



(defn CallWS
  [url body headers]
  (def options { :headers headers :body body })
  ;(println "sending data:\n" body " to url:\n" url "\n")
  (let [{:keys [status headers body error] :as resp} @(http/post url options)]
    (if error
      '("Failed, exception: " error)
      body)))

(defn call-action
  [url body]
  (CallWS url body {"Content-Type" "text/turtle"})
  )

(defn eval-form-with-context
  [form context namspcs-prefxs]
  ;(println "eval-form-with-context:\n" form)
  (if (= (first form) 'c/query)                             ;TODO: full namescpace sould be used
    (with-redefs [test-project.core/*vars* context] (eval form))))

(defn namespaces-prefixes-map-to-spaqrl
  [namspcs-prefxs-map]
  (apply str (map #(str "PREFIX " (name %) ":" " <" (namspcs-prefxs-map %) "> \n") (keys namspcs-prefxs-map)))
  )

(defn add-namespace-to-precondition-query
  [[pre-form-first pre-form-second pre-form-third] namespaces]
  (list
    pre-form-first
    pre-form-second
    (str
      (namespaces-prefixes-map-to-spaqrl namespaces)
      pre-form-third)))



(defn process-body
  "docstring"
   [plan-body]
   (let [pre (first plan-body)
         body (second plan-body)]
     (if (eval pre)
       (eval (first body))
       )
     )
  )
(defn precondition-ws-call
  [context precondition]
  ;(println "sending pre query:\n" precondition "\n context " context)
  (list context (json/read-str
     (CallWS
       "http://localhost:3030/Test"
       precondition
       {"Content-Type" "application/sparql-query" "Accept" "application/sparql-results+json ; charset=utf-8"})
     :key-fn keyword)))
;(clj-sparql.core/query {:endpoint } precondition)

;(empty? (((c/check-precondition (t/update-data-on-campaign :precondition)) :results) :bindings))
(defn get-events-response
  []
  (json/read-str
  (CallWS
    "http://localhost:3030/Test"
    "PREFIX te: <http://travelplanning.ex/Event/>
    select ?e  ?s ?type
    where  {
      ?e a te:EventType;
      te:source ?s;
      te:type ?type;
      te:processed ?processed.
       FILTER ( !?processed)} "
    {"Content-Type" "application/sparql-query" "Accept" "application/sparql-results+json ; charset=utf-8"})
  :key-fn keyword  )
  )


(defn event-processed-sparql
  [events]
  (if (not (empty? events))

    (let [eventids (map #(string/replace % "http://travelplanning.ex/Event/" "") events)] (str
       " PREFIX te: <http://travelplanning.ex/Event/>
       DELETE DATA {"
       (string/join (map #(format "te:%s  te:processed  false  . " %) eventids))
       "};
       INSERT DATA {
       "
       (string/join (map #(format "te:%s  te:processed  true  . " %) eventids))
       "}"
       ))))

(defn set-event-processed
  [events]
  (if (not (empty? events))
    (CallWS
      "http://localhost:3030/Test/update"                  ;"http://localhost:8080/flightService/webapi/W6"
      (event-processed-sparql events)
      {"Content-Type" "application/sparql-update" }) ))

(defn extract-event-iri
  [ {{ v :value} :e} ]
  v
  )

(defn process-events
  []
  (let [{{res :bindings} :results} (get-events-response)
        event-iris (map extract-event-iri res)
        ]
    (set-event-processed event-iris)
    res  ))

; we dont know what plan it is related to
(defn validate-event
  "event function in plan cue. ev is added"
  [pattern var-key type-key ev plan-name var-defs]
  (let [{{event-source :value} :s} ev
        {{event-type :value} :type} ev
        m (re-matches pattern event-source)
        ]
    (if (and (not (nil? m)) (= event-type (str "http://travelplanning.ex/Event/" (name type-key)) ))
      {:event  ((ev :e) :value)
       var-key (var-in event-source var-defs (name var-key ))
       :plan   plan-name
       }   )))

  (defn process-plan-cue-for-event
                 "takes all plans and checks their cue for particular event"
                 [plans var-defs event]
                 (->>
                    (filter #(= 'c/validate-event (first (% :cue))) plans)
                    (map #(concat (% :cue) (list event (% :name) var-defs )))        ;add to cue event data
                    (map #(eval %))                                    ;event will be evaluate
                    (remove nil?)
                      ;(map #(assoc % :plan (plan :name) )) nereikia
                      ))

(defn process-plan-cue-for-task
  [plans  task]
  (->>
    (filter #(= 'task (first (% :cue))) (vals plans))
    ;(map #(% :cue))
    (map #(u/unify (% :cue) task
                   {:task (second task)
                    :plan (% :name)
                    :unprocessed-body (% :body) }))
    ;(print-and-out)
    (remove #(= 'failed %))
    ))

(defn select-plan-candidates
  ""
    [plans var-defs events]
    (map #(process-plan-cue-for-event (vals plans) var-defs % ) events))





(defn query
  [context query-string namesps-prefxs]
  (precondition-ws-call context query-string)
  ;[var-defs context query-string & keys]
  ;(if (not (empty? keys)) ( let [k (first keys)] (string/replace query-string (name k) (var-out context var-defs (name k))))    query-string )
  )

(defn process-precondition-query-results
  [plan var-defs [context {{b :bindings} :results}]]
  (if (not (empty? b))
    (let [k (first b)]
      (->>
        ;not best way but it works for now todo:change this
        (reduce into {} (map #(hash-map
                                (keyword (str \? (name %)))
                                (var-in
                                  ((k %) :value)
                                  var-defs
                                  (str \? (name %))
                                  ))
                             (keys k)))
        (merge context)))
    ))
(defn parse-body-for-vars
  "replaces ?campaign with (test-project.core/*vars* :?campaign)"
  [body-form]
  (if (seq? body-form)
    (loop [item (first body-form)
           remaining-body (rest body-form)
           result-body ()]
      ;(println item)
      (cond
        (nil? item) result-body
        ; cia bandymas replace i reiksme - may be useful later(and (keyword? item) (variable? (name item))) (recur (first remaining-body)  (rest remaining-body)  (concat result-body (list (context-vars (keyword item)))))
        (and (symbol? item) (variable? (name item)))
        (recur
          (first remaining-body)
          (rest remaining-body)
          (concat result-body (list `(*vars* ~(keyword item)))))
        ;default
        (coll? item) (recur
                       (first remaining-body)
                       (rest remaining-body)
                       (concat result-body (list (parse-body-for-vars item)))
                       )
        :else
        (recur
          (first remaining-body)
          (rest remaining-body)
          (concat result-body (list item)))))
    body-form
    ))

(defn  eval-precondition-for-event-subscriber
  [plan var-defs event-subscr-context namspcs-prefxs]
  (let [precondition (plan :precondition)]
    (cond
      (nil? precondition) event-subscr-context
      (and (seq? precondition))
          (->>
            (eval-form-with-context (parse-body-for-vars precondition) event-subscr-context namspcs-prefxs)

            ;(precondition-ws-call)
            (process-precondition-query-results plan var-defs )

            )
      )))

(defn eval-precondition-for-event
  [plans var-defs event-context-list namspcs-prefxs]
  (->>
    (map #(eval-precondition-for-event-subscriber (plans (% :plan)) var-defs % namspcs-prefxs) event-context-list)
    ;(filter #(nil? (second %)))
    ;we will select first plan for event
    (first)
    ))



(defn eval-preconditions
  [namspcs-prefxs plans var-defs events-context-lists ]
  (->>
    (map #(eval-precondition-for-event plans var-defs % namspcs-prefxs) events-context-lists )
    (remove nil?)

    ;(print-and-out)
    ;(map #(conj ((plans (% :plan)) :precondition) %) events-context-lists)
    ;(map #(conj % 'c/query))
    ;(map #(eval %))
    ;(map #(check-precondition %))
  ))





(defn add-to-agenda
  "add event that are relevant to agent"
  [plans agenda events]
  (if (not (empty? events))
    (into agenda
         (map #(list (assoc % :unprocessed-body ((plans (% :plan)) :body))) events))
    agenda
    ))

; agenda is vector of list where each list is a stack
(defn get-stack-from-agenda
  "currently very simple - takes last item (since agenda is vector) but latter could change"
  [agenda]
  ;(list (peek agenda) (pop agenda))
  (peek agenda)
  )

;not very good definition maybe find something better later
(defn update-stack
  ""
  [statements stack stack-item]
  (let [r (rest statements)]
    (if (empty? r)
      ;remove from stack last item
      (pop stack)
      (conj (pop stack) (assoc stack-item :unprocessed-body (list true r)))))
  )

(defn get-task-instance
  [namspcs-prefxs plans var-defs task ]
  (->>
    (process-plan-cue-for-task plans task)
    (eval-precondition-for-event namspcs-prefxs plans var-defs)

    ))

(defn process-body-item
  [namspcs-prefxs plans var-defs stack]
  ;(println "stack \n" stack)
  (let [stack-item (peek stack)]
    (loop [remaining-body (stack-item :unprocessed-body)]
      (let [cond-body (first remaining-body)
            statements (second remaining-body)]
        (if (and
              (not (nil? cond-body))
              (not (nil? statements))
              (not (empty? statements))
              (not (nil? (first statements))))
           (if (eval-form-with-context namspcs-prefxs (parse-body-for-vars cond-body)  stack-item)
             ;true
             (let [stmnt (first statements)]
               (if (not (nil? stmnt))
                 ; not nil, process and return new context (with update :unprocessed-body )
                 (let [res (eval-form-with-context namspcs-prefxs (parse-body-for-vars stmnt) stack-item)]
                   ;if returned a task
                     (if (and (coll? res) (= (first res) 'task))
                       ;TODO: cia
                       (->>
                         (get-task-instance namspcs-prefxs plans var-defs res)
                         (conj (update-stack statements stack stack-item))
                         ; we to add result to stack
                         )
                       ;else
                       (update-stack statements stack stack-item)
                       )
                     ;(eval-body-form (parse-body-for-vars stmnt stack-item var-defs) var-defs)
                     )
                 ;nil - this means last item was processed - we finished but we should get to this - theoretically
                 :finished )
               )
              ;else
              (recur (nthnext remaining-body 2)))
           ; if it is empty then something is error
           :error )))))



(defn update-agenda
  "returns updated agenda - stack pushed to first position of vector. If empty removed"
  [agenda stack]
  (if (empty? stack)
    (pop agenda)
    (into (vector stack) (pop agenda)))
  )

(defn add-task
  [key & parameters]
  (concat (list 'task key ) parameters)
  )

(defn events-to-agenda
  [namspcs-prefxs plans var-defs agenda]
  (->>
    (process-events)
    (select-plan-candidates plans var-defs)
    (eval-preconditions namspcs-prefxs plans var-defs)
    (add-to-agenda plans agenda)
    )
  )

(defn before
  [date1 date2]
  (> (.getSeconds (java.time.Duration/between date1 date2)) 0)
  )

(defn date-diff-in-seconds
  [date1 date2]
  (.getSeconds (java.time.Duration/between date1 date2))
  )

(defn progress-in-agenda
  [namspcs-prefxs plans var-defs agenda]
  (if (not (empty? agenda))
    (->>
     (get-stack-from-agenda agenda)
     (process-body-item namspcs-prefxs plans var-defs)
     (update-agenda agenda))
    agenda))



;todo: idea for refacotring main could be composed of functions that can be easily executed, tested separately in repl
(defn main
  [ namspcs-prefxs plans var-defs]
  (let [started (java.time.LocalDateTime/now)]
    (println "started" started)
    (loop [agenda []
           i 0]
      (if (< i 6)
        (do
          (println i)
          (recur
            (->>
              (events-to-agenda namspcs-prefxs plans var-defs agenda)
              (progress-in-agenda namspcs-prefxs plans var-defs)
              ;(print-and-out)
              )
            (inc i)))
        agenda))
    (print (format "Ended: %s Completed:%s sec" (java.time.LocalDateTime/now) (date-diff-in-seconds started (java.time.LocalDateTime/now))))))

;(c/parse-body-for-vars (first (second (t/update-data-on-campaign :body))) (first (c/make-agenda t/plans (c/main t/plans))))
(comment
  (require '[test-project.core :as c] '[test-project.rdf :as r] '[test-project.test-plan :as t] '[test-project.match  :as u] '[test-project.sparql :as s] '[test-project.test-plan2 :as t2] '[test-project.ea :as e])
  (r/defns t "http://travelplanning.ex/")
  (r/defns xsd "http://www.w3.org/2001/XMLSchema#"))

(defn test-time
  []
  (let [started (java.time.LocalDateTime/now)]
    (println started (java.time.LocalDateTime/now)
             )
    ))

; name.precondition() name.body()
; defplan creates function that gets parameters variables and returns initialized plan with parameters and fresh variables
; plan body returns list of functions
; to progress plan algorithm should take

;{:remote-addr 0:0:0:0:0:0:0:1, :headers {host localhost:8087, user-agent PostmanRuntime/7.1.1, content-type text/turtle; charset=utf-8, content-length 2031, connection keep-alive, accept */*, accept-encoding gzip, deflate, postman-token e70660f2-c617-4b6c-8358-41ffa61df4e2, cache-control no-cache}, :async-channel #object[org.httpkit.server.AsyncChannel 0x49ad6eb /0:0:0:0:0:0:0:1:8087<->/0:0:0:0:0:0:0:1:9594], :server-port 8087, :content-length 2031, :websocket? false, :content-type text/turtle, :character-encoding utf-8, :uri /1, :server-name localhost, :query-string nil, :body #object[org.httpkit.BytesInputStream 0x5a8dbbd1 BytesInputStream[len=2031]], :scheme :http, :request-method :post}
