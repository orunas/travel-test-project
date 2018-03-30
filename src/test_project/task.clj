(ns test-project.task
  (:require [test-project.match :as u]
            [test-project.util :as util]
            [test-project.receive :as rc]
            [test-project.sparql :as s]))


(defn find-and-unify-method-instances
  [methods task]
  (->>
    ; we need to select only those that has :task key
    (filter #(% :task) (vals methods))
    ;(map #(% :cue))
    (map #(u/unify (% :task) task
                   {:task (first task)
                    :method (% :name)
                    ; TODO: not sure if it is not to early for filling unprocessed body. In event we do this when adding to agenda
                    :unprocessed-body (% :body) }))
    ;(print-and-out)
    (remove #(= 'failed %))))

(defn get-task-instance
  [methods candidate-select-fn task ]
  (->>
    (find-and-unify-method-instances methods task)
    (rc/filter-applicable-methods-on-preconditions-for-event methods)
    (util/println-and-last-out "Found for task:" (map s/context-var-to-simple-vector task) " After filter instances:")
    (candidate-select-fn)))


(defn add-task
  [key & parameters]
  (concat (list 'task key ) parameters))

