(ns test-project.task
  (:require [test-project.match :as u]
            [test-project.util :as util]
            [test-project.receive :as rc]
            [test-project.sparql :as s]))


(defn find-and-unify-method-instances
  [a-methods a-task]
  (->>
    ; we need to select only those that has :task key
    (filter #(% :task) (vals a-methods))
    ;(map #(% :cue))
    (map #(u/unify (% :task) a-task
                   {:task (first a-task)
                    :method (% :name)
                    ; TODO: not sure if it is not to early for filling unprocessed body. In event we do this when adding to agenda
                    :unprocessed-body (% :body) }))
    ;(print-and-out)
    (remove #(= 'failed %))))

(defn get-task-instance
  [a-methods candidate-select-fn a-task ]
  (->>
    (find-and-unify-method-instances a-methods a-task)
    (rc/filter-applicable-methods-on-preconditions-for-event a-methods)
    (util/println-and-last-out "Found for task:" (map s/context-var-to-simple-vector a-task) " After filter instances:")
    (candidate-select-fn)))


(defn add-task
  [key & parameters]
  (concat (list 'task key ) parameters))

