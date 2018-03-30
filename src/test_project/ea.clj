(ns test-project.ea
  (:require [test-project.rdf :as r]
            [test-project.ws :as ws]
            [test-project.util :as u]
            [test-project.sparql :as s]
            [test-project.task :as t]
            [test-project.receive :as rc]
            [test-project.jena :as j] :reload))

; need to remove somewhere

(defn simplify-stack-for-print [stack]
  (map s/simplify-context-for-print stack))

(defn simplify-agenda-for-print [agenda]
  (map simplify-stack-for-print (:stacks agenda)))

(defn apply-val-f
  [f1 a-var & params]
  (assoc a-var :value (apply f1 (a-var :value) params)) )

(defn before
  [date1 date2]
  (> (.getSeconds (java.time.Duration/between date1 date2)) 0)
  )

(defrecord variable [value prefix-ns type] )

(def methods-lib (atom {}) )


(defmacro def-method
  [name parameters & {:keys [event task namespaces methods actions precondition body] :or {methods 'test-project.ea/methods-lib}}]
  (comment `(def ~name {:event        (s/build-event ~namespaces (quote ~event))
                :precondition (s/build-pre-query ~namespaces ~(set parameters) ~precondition)
                :body         ~body}))
  (let [needEvent (not (nil? event))]
    `(swap! ~methods
            assoc
            (keyword (quote ~name))
            {:name         (keyword (quote ~name))
             :task         (quote ~task)
             :actions       ~actions
             :event        (if ~needEvent (s/build-event ~namespaces (quote ~event)))
             :precondition (s/build-precondition ~namespaces ~(set parameters) ~precondition)
             :body         ~body})))

(defn add-to-agenda
  "add event that are relevant to agent
  Events is vector where each event is binding/context with variables bound
  a-method-lib is map of methods definitions where key is method name
  Agenda is vector, where each item is same binding/context extended with :unprocessed-body.
  Initially that :unprocessed-body is method's body"
  [a-method-lib agenda events]
  (if (not (empty? events))
    ; list is because we use list for stack implementation empty list - empty stack
    (let [added (map #(conj '() (assoc % :unprocessed-body ((a-method-lib (% :method)) :body))) events)]
      ;(println "Adding to agenda:" added)
      {:stacks (into (agenda :stacks) added)})
    agenda ))

; agenda is vector of list where each list is a stack
(defn get-stack-from-agenda
  "currently very simple - takes last item (since agenda has on stacks which is vector) but latter could change"
  [{stacks :stacks}]
  ;ideally it should have selection function how to choose from stacks
  ; currenly it will act as queue
  (peek stacks) )

;not very good definition maybe find something better later
(defn update-stack
  ""
  [statements stack stack-item]
  (let [r (rest statements)]
    (if (empty? r)
      ;remove from stack last item
      (pop stack)
      (conj (pop stack) (assoc stack-item :unprocessed-body [true r])))))

(defn eval-form-with-context
  [form vars]
  ; form should be
  ;  either function with one variable (then we call it)
  ;  or simple statement that is not referring to context (then just eval it)
  (if (fn? form)
    (form vars)
    (eval form)))


(defn process-body-item
  "processed on item in body
  Returns remainder"
  [methods candidate-select-fn stack]
  ;(println "stack \n" stack)
  (let [stack-item (peek stack)]
    ; loop in case conditional is false, then we select second
    (loop [remaining-body (stack-item :unprocessed-body)]
      ; every time first comes conditional and then follows list of statement
      (let [cond-body (first remaining-body)
            statements (second remaining-body)]
        (if (and
              (not (nil? cond-body))
              (not (nil? statements))
              (not (empty? statements))
              (not (nil? (first statements))))
          (if (eval-form-with-context cond-body  stack-item)
            ;true
            (let [stmnt (first statements)]
              (if (not (nil? stmnt))
                ; not nil, process and return new context (with update :unprocessed-body )
                (let [res (eval-form-with-context stmnt stack-item)]
                  ;if returned a task
                  (cond
                    (and (coll? res) (= (first res) 'task))
                      ;TODO: cia
                      (do
                        (println "Found task:" (map s/context-var-to-simple-vector res))
                        (if-let [candidate (t/get-task-instance methods candidate-select-fn (rest res))]
                          [(conj (update-stack statements stack stack-item) candidate) nil]
                          (u/println-and-last-out "Error: not candidates for task found" [nil :error-no-candidates-for-task])))
                    (keyword? res)                          ;keyworod means we got an error
                      [nil res]
                    :else
                      [(update-stack statements stack stack-item) nil]
                    )
                  ;(eval-body-form (parse-body-for-vars stmnt stack-item var-defs) var-defs)
                  )
                ;nil - this means last item was processed - we finished but we should get to this - theoretically
                [nil :finished])
              )
            ;else we take another branch
            (recur (nthnext remaining-body 2)))
          ; if it is empty then something is error
          [nil :error])))))

(defn update-agenda
  "returns updated agenda - stack pushed to first position of vector. If empty removed"
  [agenda [stack error]]
  ; agent is map with key stack which point o a vector of stack
  (if error
    [nil error]
    [{:stacks
      (if (empty? stack)
        (pop (agenda :stacks))
        (into (vector stack) (pop (agenda :stacks))))} nil]))

(defn events-to-agenda
  [a-method-lib candidate-select-fn agenda]
  (let [events (rc/get-events)]
    (println "received events:" events)
    (->>
      (rc/find-all-relevant-methods-for-all-events (vals a-method-lib) events)
      ;(u/println-and-last-out "found relevant methods:")
      ; last param function - currently we get only first
      (rc/filter-applicable-methods-on-preconditions a-method-lib candidate-select-fn)
      (rc/filter-and-remove-unhandled-events events)
      ;(u/println-and-last-out "filtered applicable methods:")
      (add-to-agenda a-method-lib agenda)
      ;(u/pprintln-and-out "Agenda after events:")
      )))

(defn progress-in-agenda
  "progressed on step in agenda and returns updated agenda"
  [namspcs-prefxs methods candidate-select-fn agenda]
  ;(print agenda)
  (if (not (empty? (agenda :stacks)))
    (->>
      (get-stack-from-agenda agenda)
      ; (u/pprintln-and-out "got stack from agenda:" simplify-stack-for-print)
      ; form this we return vector [data error]
      (process-body-item methods candidate-select-fn)
      (update-agenda agenda)
      ;(u/pprintln-and-out "Agenda update after step:")
      )
    [agenda nil]))

(defn main
  ([namspcs-prefxs a-method-lib max-limit] (main namspcs-prefxs a-method-lib max-limit {:stacks []}))
  ([namspcs-prefxs a-method-lib max-limit agenda-param]

   (if (empty? a-method-lib)
      (println "Empty methods library")
      (let [started (java.time.LocalDateTime/now)
            candidate-select-fn #(first %)]
        (println "started" started)
        (->> (loop [i 0   i-agenda agenda-param]
               (if (< i max-limit)
                 (do
                   (println "step" i)
                   (let [agnd (events-to-agenda a-method-lib candidate-select-fn i-agenda)
                         res (progress-in-agenda namspcs-prefxs a-method-lib candidate-select-fn agnd)]
                     (if (nil? (second res))
                       (recur
                         (inc i)
                         (first res))
                       (println "stopped on error:" (second res)))))
                 i-agenda))
             (u/pprintln-and-out "Completed. Agenda:" simplify-agenda-for-print))
        (print (format "Ended: %s Completed:%s sec" (java.time.LocalDateTime/now) (u/date-diff-in-seconds started (java.time.LocalDateTime/now))))))))

(defn call-action
  ([url] (call-action url nil))
  ([url body]
   (println "Action calls url:" url)
   (ws/CallWS url (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" body)
              {"Content-Type" "text/turtle"})))









