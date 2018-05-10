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


(defn eval-form-with-context
  [form vars]
  ; form should be
  ;  either function with one variable (then we call it)
  ;  or simple statement that is not referring to context (then just eval it)
  (if (fn? form)
    (form vars)
    (eval form)))



(defn set-step-active-on-map
  [agenda-map step-keyword]
  (assoc agenda-map :normal-step-keys (remove #{step-keyword} (:normal-step-keys agenda-map) )
                    :active-step-keys (conj (:active-step-keys agenda-map) step-keyword)))


(defn remove-step
  ([agenda-map node-keyword ] (remove-step agenda-map node-keyword nil))
  ([{:keys [intention-graph normal-step-keys active-step-keys] :as agenda-map} node-keyword child-node-keyword]
    ;(println node-keyword child-node-keyword)
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
           (if (node :steps-ordered)
             ; if exists and they are ordered we add next one to normal-step-keys add remove child from :steps
             (assoc agenda-map
               :intention-graph (update-in intention-graph [node-keyword] #(assoc % :steps new-steps))
               :normal-step-keys (conj normal-step-keys (first new-steps)))
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
  [a-methods actions candidate-select-fn agenda-atom]
  ;(println (step-keyword intention-graph))
  (let [step-keyword (-> @agenda-atom :normal-step-keys first )
        ; new-steps (subvec step-ids 1)
        step-node (-> @agenda-atom :intention-graph step-keyword )
        stmnt (step-node  :body )
        res (eval-form-with-context stmnt step-node)]
    ;if returned a task
    ; (println a-methods "t" res)
    (cond
      (and (coll? res) (= (first res) 'task))
        (if-let [candidate (t/get-task-instance a-methods candidate-select-fn (rest res))]
          ;[(conj (update-stack statements stack stack-item) candidate) nil]
          [(add-expanded-method-instance-to-agenda a-methods agenda-atom candidate (:id step-node)) nil]
          (u/println-and-last-out "Error: not candidates for task found" [nil :error-no-candidates-for-task]))
      (and (coll? res) (= (first res) :action))
        [(let [c (chan)]
           (go
             (swap! agenda-atom set-step-active-on-map step-keyword)
             (let [action-key (second res)
                   params (nth res 2)]
               (println "Executing action" res)
               ;  (methods)
               (>! c (apply (action-key actions) params))))
           (go
             (println "finished" (<! c))
             (swap! agenda-atom remove-step step-keyword)))
         nil]
      (keyword? res)                                        ;keyword means we got an error
        [nil res]
      (nil? res)
        [(swap! agenda-atom remove-step step-keyword) nil]
      :else
        [res :error-unexpected-response]
      ;[(update-stack statements stack stack-item) nil]
      )
    ;(eval-body-form (parse-body-for-vars stmnt stack-item var-defs) var-defs)
    ))


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


(defn main
  ([namspcs-prefxs a-method-lib actions max-limit] (main namspcs-prefxs
                                                         a-method-lib
                                                         actions
                                                         max-limit
                                                         (atom {:intention-graph {:r0 {:type :root :id :r0}} :normal-step-keys () :active-step-keys ()})))
  ([namspcs-prefxs a-method-lib actions max-limit agenda-atom]
   (if (empty? a-method-lib)
      (println "Empty methods library")
      (let [started (java.time.LocalDateTime/now)
            active-actions-limit 3
            candidate-select-fn #(first %)
            ]
        (println "started" started)
        (->> (loop [i 0   ]
               (if (< i max-limit)
                 (do
                   (println "step" i)
                   (let [agnd (events-to-agenda a-method-lib candidate-select-fn agenda-atom)]
                     (if (and
                           (not (empty? (:normal-step-keys @agenda-atom)))
                           (<= (count (:active-step-keys @agenda-atom)) active-actions-limit))
                       ; if there steps and max limit active not exceeded progress in agenda
                       (let [[_ err] (process-step-node a-method-lib actions candidate-select-fn agenda-atom)]
                         (if (nil? err)
                              (recur (inc i))
                              (println "stopped on error:" err)))
                       (do
                         (println "skipping. no normal steps or active limit exceeded")
                         (recur (inc i))))))
                 agenda-atom))
             (u/pprintln-and-out "Completed. Agenda:" simplify-agenda-for-print))
        (print (format "Ended: %s Completed:%s sec" (java.time.LocalDateTime/now) (u/date-diff-in-seconds started (java.time.LocalDateTime/now))))))))










