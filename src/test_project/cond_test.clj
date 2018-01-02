(ns test-project.cond-test
  (:require [test-project.rdf :as r]))

;Simple test, that our scenario works where each statement is a function with parameters vars and var-defs
; 1 conditional and two statements (before ?start ?end) ((print ?start) (print ?end))

(def a12
  (list
    (fn [vars var-defs] true)
    (list
      (fn [vars var-defs]
        (print "first step: printing campaing" (vars :campaign)))
      (fn [vars var-defs]
        (print "second step. will return now in rdf format:" (r/to-rdftype (r/now)))
        (r/to-rdftype (r/now)))
      )))

(defn test-simple-al
  [vars var-defs fn-l]
  (if ((first fn-l) vars var-defs)
    (loop [items (second fn-l)]
      (if (empty? items)
        (print "Done!")
        (do
          ((first items) vars var-defs)
          (recur
            (rest items)
            )
          )
        )
      )
    (print "first cond false")
    )
  )



(comment
  "setup"
  (require '[test-project.cond-test :as tc])
  (tc/test-simple-al bin t/variables-defs tc/a12)
         )