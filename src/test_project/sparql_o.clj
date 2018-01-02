(ns test-project.sparql-o
  (:require [test-project.core :as c] [test-project.rdf :as r] :reload))



(defn process-triple-element-binding
  "take element and if bound variable substitutes value. Otherwise outputs value"
  [context var-defs aliteral]
  (let [aliteral-name (name aliteral)]
    `(if (c/variable? ~aliteral-name)
       (let [var-key (keyword (c/var-name ~aliteral-name))
             var-val (context var-key)]
         ;(print var-key)
         (if (nil? var-val)
           ~aliteral-name
           (c/var-out ~context ~var-defs var-key)))             ;TODO: not efective
       ~aliteral-name
       )))


(defmacro process-triple-element-binding2
  "take element and if bound variable substitutes value. Otherwise outputs value"
  [context var-defs aliteral]
  (let [aliteral-name (name aliteral)]
    `(if (c/variable? ~aliteral-name)
     (let [var-key# (keyword (c/var-name ~aliteral-name))
           var-val# (~context var-key#)]
       ;(print var-key#)
       (if (nil? var-val#)
         ~aliteral-name
         (c/var-out ~context ~var-defs var-key#)))             ;TODO: not efective
     ~aliteral-name
     )))


(defn triple
  [subject predicate object]
  (let
    [context (gensym "context-")
     var-defs (gensym "var-defs-")
     trpl (gensym "triple-")
     ]
    ;(print &env)
    `(fn
       [~context ~var-defs]
       (str
         ;(~context ~(keyword subject))
         (process-triple-element-binding2 ~context ~var-defs ~subject)
         " "
         (process-triple-element-binding2 ~context ~var-defs ~predicate)
         " "
         (process-triple-element-binding2 ~context ~var-defs ~object)
         " . \n"
         ))))

(defmacro triplem
  [subject predicate object]
  (triple subject predicate object)
  )


(defmacro tripleSameSubj
  "not finished yet. should take subject and pairs (predicate, object) and returns in SPARQL format.
  Format like turtle, but additionally handles variable by substituting bound vars and leaving out unbound symbols."
  [subject & items]
  (let
    [context (gensym "context-")
     var-defs (gensym "var-defs-")
     trplSameSubj (gensym "tripleSameSubj-")
     items-by-2 (partition 2 items)
     triples (map #(triple subject (first %) (second %)) items-by-2)
     ]
    ;(print triples)
    `(fn                                                    ;~trplSameSubj
       [~context ~var-defs]
       ;(map #(triple ~subject ~(first items-by-2) ~(second items-by-2)) ~items-by-2)
       (str ~@(map #(list % context var-defs) triples))
       )))

(defn process-query-filter
  [context var-defs forms output]
  (if (list? forms)
    (let [item (first forms)]
      (cond
        (and (symbol? item) (= item 'and))
          (concat  output
                   '("(")
                   (process-query-filter context var-defs (second forms) [])
                   '(") && (")
                   (process-query-filter context var-defs (nth forms 2) [])
                   '(")"))
        (and (symbol? item)
             (or (= (name item) "<") (= (name item) ">") (= (name item) "=")))
          (concat output
                  (process-query-filter context var-defs (second forms) [])
                  (list " " (name item) " ")
                  (process-query-filter context var-defs (nth forms 2) []))
        (symbol? item)
        (conj output forms)
        ))
    ;(and (symbol? item) (variable? item)) veliau
    (conj output (process-triple-element-binding context var-defs forms))
    ))

(defn parse-where-subgrph-pattern
  [context var-defs [subject & remaining]]
  (loop [rem remaining n3 ""]
    (if (empty? rem)
      n3
      (let [pred (first rem) obj (second rem)]
        (recur
          (nthrest rem 2)
          (str
            n3
            (process-triple-element-binding context var-defs subject) " "
            (process-triple-element-binding context var-defs pred) " "
            (process-triple-element-binding context var-defs obj) ".\n"))))))

(defn parse-where-items1
  [context var-defs items output]
  (let [item (first items) ]
    (cond
      (list? item)(recur
                    context
                    var-defs
                    (rest items)
                    (str output "\n" (parse-where-subgrph-pattern context var-defs item)))
      (keyword? item)
      (if (= (name item) "filter")
        (str
          output
          (process-query-filter context var-defs (second items) (str " FILTER ("))
          " )")
        output
        )                                 ; later
      )))

; now don't know exactly how it works therefore leave out
(defn query
  [context var-defs {:keys [select where]}]
  ;(print where)
  (str
    "select  "
    (clojure.string/join " " (map name select))
    " \n"
    " where {"
    ;(clojure.string/join " " (map #(clojure.string/join "; " %) where))
    (parse-where-items1 context var-defs where "")
    "}"
    ))

(defmacro build-pre-query
  "idea that takes structure and creates function like t/string-query-example,
  which has sparql as string with placeholders for bound variable"
  [{:keys [select where]}]
  ;(print where)
  (str
    "select  "
    (clojure.string/join " " (map name select))
    " \n"
    " where {"
    ;(clojure.string/join " " (map #(clojure.string/join "; " %) where))
    ; (parse-where-items1 context var-defs where "")
    "}"
    ))









(comment sparql-query3
  [data]
  (str
    "select  "
    (clojure.string/join " " (map name select))
    " \n"
    )
  )