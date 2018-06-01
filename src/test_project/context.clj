(ns test-project.context)
;namespace for managing context variables

(defn var-val [context var]
  (if-let [var-c (context var)]
    (var-c :value)))
