(ns test-project.ea
  (:require [test-project.rdf :as r]
            [test-project.ws :as ws]
            [test-project.util :as u]
            [test-project.sparql :as s]
            [test-project.task :as t]
            [test-project.receive :as rc]
            [clojure.core.async :as async :refer [>! <! >!! <!! go chan]]
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


(defmacro
  def-method
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


(defn extract-steps [a-method-lib parent-method-map]
  (let [body (:body (a-method-lib (:method parent-method-map)))
        ordered (vector? body)
        steps (map #(assoc parent-method-map
                      :parent (:id parent-method-map)       ;parent is updated id
                      :type :step
                      :body %
                      :id (keyword (gensym "s"))
                      ) body)
        step-ids (map #(:id %) steps)]
    [(conj steps
           (assoc parent-method-map :steps step-ids
                                    :steps-ordered ordered))
     (if ordered (vector (first step-ids)) step-ids) ]))

(defn multiple-update-in
  "mp - map
  keys-fs-vals - vector [:key f val] whe"
  [mp fs-keysv-vals]
  (reduce #(update-in %1 (second %2) (first %2) (nth 2 %2 )) mp fs-keysv-vals))

(defn update-agenda-data
  [agenda-map new-node-map step-keys-to-add step-key-to-remove]
  ;(println new-node-map)
  (->
    agenda-map
    (update-in  [:intention-graph] merge new-node-map)
     (update-in [:normal-step-keys] #(remove %2 %1) #{step-key-to-remove})
     (update-in [:normal-step-keys] concat step-keys-to-add)))



(comment defn add-event-to-agenda
  "Add event that are relevant to agent.
  Events is vector where each event is binding/context with variables bound
  a-method-lib is map of methods definitions where key is method name
  Agenda-atom hold map.
  1st is a graph that is stored as map, where each node has references (by keyword) to other nodes. Node data is same binding/context.
  2nd is collection of active steps."
  [a-method-lib agenda-atom events]
  (if (not (empty? events))
    ; list is because we use list for stack implementation empty list - empty stack
    (let [[nodes step-ids]
          (->> (map #(assoc % :type :method :parent :r0 :id (keyword (gensym "m")) :steps []) events)
               (map #(extract-steps a-method-lib %))        ; list of vectors [stepnodes stepids]
               (reduce #(list (concat (first %1) (first %2)) (concat (second %1) (second %2)))) ; list of maps
               ;(reduce #(assoc %1 (:id %2) %2) (first agenda))
               ) ]
      (do
        (swap! agenda-atom #(update-in %1 [:intention-graph] merge  %2)) (reduce #(assoc %1 (:id %2) %2) {} nodes)
        (swap! agenda-atom #(update-in %1 [:normal-step-keys] concat %2) step-ids)))
    ;(let [added ]       {:stacks (into (agenda :stacks) added)})
    agenda-atom ))

(defn get-new-steps
  [a-method-lib method-instance parent-keyword]
  (->> (assoc method-instance :type :method :parent parent-keyword :id (keyword (gensym "m")) :steps [])
       (extract-steps a-method-lib)                   ; vector [stepnodes stepids]
       ;(reduce #(assoc %1 (:id %2) %2) (first agenda))
       ))

(defn add-expanded-method-instance-to-agenda
  "add expanded (with steps as child nodes
  method-instance is a context/binding map"
  [a-method-lib a-agenda-atom method-instance parent-keyword]
  ; (print "added")
  (let [[nodes new-step-ids] (get-new-steps a-method-lib method-instance parent-keyword)
        nodes-map (reduce #(assoc %1 (:id %2) %2) {} nodes)]
    ;[nodes-map new-step-ids]
    (swap! a-agenda-atom update-agenda-data nodes-map new-step-ids parent-keyword)))


(defn add-events-to-agenda
  ;([a-method-lib agenda-atom events]   (add-event-to-agenda a-method-lib agenda-atom (first events) (next events)))
  ([a-method-lib agenda-atom events]
   (let [curr-event (first events)]
     (if (nil? curr-event)
      @agenda-atom
      (do
        (add-expanded-method-instance-to-agenda a-method-lib agenda-atom curr-event :r0)
        (add-events-to-agenda a-method-lib agenda-atom (rest events)))))))


  ; agenda is vector of list where each list is a stack
  ; probably don't need this anymore
(defn get-stack-from-agenda
  "currently very simple - takes last item (since agenda has on stacks which is vector) but latter could change"
  [{stacks :stacks}]
  ;ideally it should have selection function how to choose from stacks
  ; currenly it will act as queue
  (peek stacks))

;not very good definition maybe find something better later
  ;obsolete. todo: remove after migration to graph
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


; old version should be removed when new method is added
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


(defn set-step-active-on-map
  [agenda-map step-keyword]
  (assoc agenda-map :normal-step-keys (remove #{step-keyword} (:normal-step-keys agenda-map) )
                    :active-step-keys (conj (:active-step-keys agenda-map) step-keyword)))

(defn select-removable-nodes
  [g k]
  (let [parent-key (-> g k :parent)]
    (if (empty? (-> g parent-key :steps))
      parent-key
     ))
  )

(comment (dissoc-if graph-map #(empty? (-> % parent-key :steps)) parent-key))
(defn dissoc-if
  [m f k]
  (if (apply f (list m)) (dissoc m k) m))

(defn remove-parents [graph-map parent-key]
  (if (or (empty? (-> graph-map parent-key :steps)) (= parent-key :r0))
    ; hierarchy step -> method -> step
    (->
      ; first remove
      (dissoc graph-map parent-key)
      (remove-parents (:parent (graph-map parent-key))))
    ; else do nothing just return map unchanged
    graph-map
    ))

(comment (-> intention-graph
             ;todo changes here
             (dissoc step-keyword)
             (update-in [parent-key :steps] #(remove #{step-keyword} %))
             (remove-parents parent-key))

         )

(defn remove-step
  ([agenda-map node-keyword ] (remove-step agenda-map node-keyword nil))
  ([{:keys [intention-graph normal-step-keys active-step-keys] :as agenda-map} node-keyword child-node-keyword]
   (println node-keyword child-node-keyword)
   (let [node (intention-graph node-keyword)
         parent-key (node :parent)]
     ; when step is removed we need add steps that waited for completion
     (case (node :type)
       ; if we have method we check whether more steps exists
       :method
       (let [new-steps (remove #{child-node-keyword} (node :steps))]
         (if (empty? new-steps)
           ;   if not exists means we done with method. Remove it and check parent
           (-> (update-in agenda-map [:intention-graph] #(dissoc % node-keyword))
               (remove-step parent-key node-keyword)
               )
           (if (node :step-ordered)
             ; if exists and they are ordered we add next one to normal-step-keys add remove child from :steps
             (assoc agenda-map
               :intention-graph (update-in intention-graph [node-keyword] #(assoc % :steps new-steps))
               :normal-step-keys (conj normal-step-keys (-> node :steps first)))
             ; if exists and not ordered then just remove from steps
             (update-in agenda-map [:intention-graph node-keyword ] #(assoc % :steps new-steps)))))
       ; if we have step we remove it and recursively check parent
       :step
       (-> (assoc agenda-map :intention-graph (dissoc intention-graph node-keyword)
                             :normal-step-keys (remove #{node-keyword} normal-step-keys)
                             :active-step-keys (remove #{node-keyword} active-step-keys))
           (remove-step parent-key node-keyword)
           )
       :root agenda-map
       agenda-map
       ))))

; should just evaluate function
(defn process-step-node
  ""
  [methods candidate-select-fn agenda-atom]
  ;(println (step-keyword intention-graph))
  (let [step-keyword (-> @agenda-atom :normal-step-keys first )
        ; new-steps (subvec step-ids 1)
        step-node (-> @agenda-atom :intention-graph step-keyword )
        stmnt (step-node  :body )
        res (eval-form-with-context stmnt step-node)]
    ;if returned a task
    (cond
      (and (coll? res) (= (first res) 'task))
        ;TODO: cia
        (do
          ; (println "Found task:" (map s/context-var-to-simple-vector res))
          (if-let [candidate (t/get-task-instance methods candidate-select-fn (rest res))]
            ;[(conj (update-stack statements stack stack-item) candidate) nil]
            (add-expanded-method-instance-to-agenda methods agenda-atom candidate (:id step-node))
            (u/println-and-last-out "Error: not candidates for task found" [nil nil :error-no-candidates-for-task])))
      (and (coll? res) (= (first res) :action))
        (let [c (chan)]
          (go
            (swap! agenda-atom set-step-active-on-map step-keyword)
            (println "Executing action" res)
            (>! c "done"))
          (go
            (println "finished" (<! c))
            (swap! agenda-atom remove-step step-keyword)))
      (keyword? res)                                        ;keyworod means we got an error
        [nil res]
      (nil? res)
        (swap! agenda-atom remove-step step-keyword)
      :else
        [nil res :error-unexpected-response]
      ;[(update-stack statements stack stack-item) nil]
      )
    ;(eval-body-form (parse-body-for-vars stmnt stack-item var-defs) var-defs)
    ))

(defn update-agenda-old
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
  [a-method-lib candidate-select-fn agenda-atom]
  (let [events (rc/get-events)]
    (println "received events:" events)
    (->>
      (rc/find-all-relevant-methods-for-all-events (vals a-method-lib) events)
      ;(u/println-and-last-out "found relevant methods:")
      ; last param function - currently we get only first
      (rc/filter-applicable-methods-on-preconditions a-method-lib candidate-select-fn)
      (rc/filter-and-remove-unhandled-events events)
      ;(u/println-and-last-out "filtered applicable methods:")
      (add-events-to-agenda a-method-lib agenda-atom)
      ;(u/pprintln-and-out "Agenda after events:")
      )))

(defn progress-in-agenda
  "progressed on step in agenda and returns updated agenda"
  [namspcs-prefxs methods candidate-select-fn agenda-atom]
  ;(print step-ids)
  (if (not (empty? (:normal-step-keys @agenda-atom)))
    (process-step-node methods candidate-select-fn agenda-atom)
    ;    (->>
      ;(get-stack-from-agenda agenda)
      ; (u/pprintln-and-out "got stack from agenda:" simplify-stack-for-print)
      ; form this we return vector [data error]
      ; (update-agenda agenda)
      ;(u/pprintln-and-out "Agenda update after step:")      )
    agenda-atom))

(defn main
  ([namspcs-prefxs a-method-lib max-limit] (main namspcs-prefxs a-method-lib max-limit (atom {:intention-graph {:r0 {:type :root :id :r0}}})))
  ([namspcs-prefxs a-method-lib max-limit agenda-atom]

   (if (empty? a-method-lib)
      (println "Empty methods library")
      (let [started (java.time.LocalDateTime/now)
            candidate-select-fn #(first %)
            ]
        (println "started" started)
        (->> (loop [i 0   ]
               (if (< i max-limit)
                 (do
                   (println "step" i)
                   (let [agnd (events-to-agenda a-method-lib candidate-select-fn agenda-atom)
                         res (progress-in-agenda namspcs-prefxs a-method-lib candidate-select-fn agnd)]
                     (if (nil? (second res))
                       (recur
                         (inc i)                  )
                       (println "stopped on error:" (second res)))))
                 agenda-atom))
             (u/pprintln-and-out "Completed. Agenda:" simplify-agenda-for-print))
        (print (format "Ended: %s Completed:%s sec" (java.time.LocalDateTime/now) (u/date-diff-in-seconds started (java.time.LocalDateTime/now))))))))

(defn call-action
  ([url] (call-action url nil))
  ([url body]
   (println "Action calls url:" url)
   (ws/CallWS url (str "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n" body)
              {"Content-Type" "text/turtle"})))

(defn add-action
  [key & parameters]
  (concat (list :action key ) parameters))







