(ns test-project.sparql
  (:require
    ;[test-project.core :as c]
    [test-project.context :as ctx]
            [test-project.rdf :as r] :reload))

;here we make an assumption that we will have list of parameters
; like '(?campaign ?dateEnd). It can be different structure - like map
; and namespaces, type should be included with variables in same binding - not anywhere else (like now we have variable-defintions), namespaces-prefixes still should be separate)
;(defn )



(defn insert-context-to-inner-expression
  "recursive check all expressions and add context if need"
  [context items]
  (if (coll? items)
    (let [f (first items)]
      (if (contains? '#{s/and s/> s/< s/in s/f-and s/f> s/f< s/f-in} f)
        (concat (list f context) (map #(insert-context-to-inner-expression context %) (rest items)))
        items))                                             ;
    items))

(defn insert-context-to-inner-expression-and-apply
  "recursive check all expressions and add context if need"
  [context items]
  ; (println items)
  (if (coll? items)
    (let [f (first items)]
      ;  (println f context)
      (if (contains? '#{s/f-and s/f> s/f< s/f-in} f)
        (apply
          (resolve f)
          context
          (map #(insert-context-to-inner-expression-and-apply context %) (rest items)))
        items))                                             ;
    items))

;if simple var (not list) then we need just check where it is among paremeters
; if it is among parameters wrap for output and change to vars map key lookup.
; If not it is a fresh variable that will be bound
; othervise some error.
;if list it is a function that will deliver result.
;     We need to wrap.
;     Inside function at any could be variables that needs to be replaced to vars map key lookup

(defn replace-var-to-varskeymap-lookup-rec
  "recursivelly searches statements and looks for variables and replaces them to vars keymap lookup"
  ([{:keys [params vars-name]} item] (replace-var-to-varskeymap-lookup-rec params vars-name item))
  ([params vars-name item ]
   (cond
     (and (coll? item) (not (empty? item)))
     (concat
       (list (replace-var-to-varskeymap-lookup-rec params vars-name (first item)))
       (replace-var-to-varskeymap-lookup-rec params vars-name (rest item)))
     (and (r/variable? item) (contains? params item))
     (list `ctx/var-val vars-name (keyword item))
     :else
     item)))


(defn wrap-with-rdf-output-and-replace-vars
  [{:keys [params vars-name]} item]
  (if (or (coll? item) (and (r/variable? item) (contains? params item) ))
    (list `r/to-rdftype
          (replace-var-to-varskeymap-lookup-rec params vars-name item))
    (str item)))

;primitive operations start

;ConditionalAndExpression
(defn f-and
  ([context & items]
   `(str
      "("
      (clojure.string/join " && \n " (list ~@items)            ;(insert-context-to-inner-expression ~context ~items)
                           )
      ")")))

(defmacro m-and
  [context & items]
  `(f-and ~context ~@items))


(defn f<
  [context item1 item2]
  ;(println "f<" item1)
  `(str
     "("
     ~(wrap-with-rdf-output-and-replace-vars context item1)
     " < "
     ~(wrap-with-rdf-output-and-replace-vars context item2)
     ")"))

; one of relational expressions. consist of 2 numeric expressions. lets say is consist only of primary expressions
(defmacro m<
  [context item1 item2]
  (f< context item1 item2)
)

(defn f>
  [context item1 item2]
  (f< context item2 item1)
  )

(defn f-in
  [context var1 in-items]
  `(str
    "("
    ~(str var1)
    " in ("
    (clojure.string/join "," ~in-items)
    "))"
    )
  )

(defmacro m>
  [context item1 item2]
  (< context item2 item1))

(defmacro m-in
  "currently in only represents is sparql"
  [context var1 in-items]
  (f-in context var1 in-items)
  )

;primitive operartions end

(defn buildfilter
  [context items]
  ;(print items)
  (if (not (empty? items))
    `(" FILTER "
       ~(insert-context-to-inner-expression-and-apply context items))))

(defmacro buildfilterfn
  [context items]
  ;(print items)
  `(fn [~(context :vars-name)]
    ~(insert-context-to-inner-expression-and-apply context items)))

; end new gen *********************

; these  */*/*/* are used however not sure whether it good location for them



(defn var-short-val-out [var-ctx]
  (case (var-ctx :type)
    :uri (var-ctx :value)
    :date (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_DATE))
    :dateTime (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_INSTANT))
    :literal (case (var-ctx :datatype)
               "http://www.w3.org/2001/XMLSchema#dateTime" (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_INSTANT))
               "http://www.w3.org/2001/XMLSchema#date" (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_DATE))
               (var-ctx :value))
    (var-ctx :value)))

(defn var-full-val-out [var-ctx]
  (if (= (var-ctx :type) :uri)
    (str (var-ctx :prefix-ns) (var-ctx :value))
    (var-short-val-out var-ctx)))



(defn var-out
  "we expected var as keyword like :?campaign"
  ([context var] (if-let [v (context var)] (var-out v) (println "Error: var is nil" var)))
  ([var-ctx]
   (case (var-ctx :type)
     :uri (str "<" (var-ctx :prefix-ns) (var-ctx :value) ">")
     :literal (case (var-ctx :datatype)
                "http://www.w3.org/2001/XMLSchema#string" (r/literal (var-ctx :value) :type "<http://www.w3.org/2001/XMLSchema#string>")
                "http://www.w3.org/2001/XMLSchema#dateTime" (r/literal (.format
                                                                         (var-ctx :value)
                                                                         (if (= (type (var-ctx :value)) java.time.ZonedDateTime)
                                                                           (java.time.format.DateTimeFormatter/ISO_INSTANT)
                                                                           (java.time.format.DateTimeFormatter/ISO_LOCAL_DATE_TIME)))
                                                                       :type "<http://www.w3.org/2001/XMLSchema#dateTime>")
                ; use substring because wan time to be  at UTC zone
                "http://www.w3.org/2001/XMLSchema#date" (r/literal (.format (var-ctx :value) (java.time.format.DateTimeFormatter/ISO_DATE))
                                                                   :type "<http://www.w3.org/2001/XMLSchema#date>")
                "xsd:integer" (var-ctx :value)
                "xsd:decimal" (var-ctx :value)
                "http://www.w3.org/2001/XMLSchema#integer" (var-ctx :value)
                "http://www.w3.org/2001/XMLSchema#decimal" (var-ctx :value)))))

(defn datetime-type-to-date
  [context a-var]
  (if-let [var-ctx (context a-var)]
    (assoc var-ctx :datatype "http://www.w3.org/2001/XMLSchema#date")))


; end */*/*/*




(defn process-triple-element-to-sparql-element
  "triple element to sparql string element"
  [context-v params element]
  (if (and (r/variable? element) (contains? params element))
    `(var-out ~context-v ~(keyword element))
    (str element)))

(defn process-where-subgrph-pattern2
  "here we go recursively through all items in Triples block"
  ([context-v params [subject & remaining]] (process-where-subgrph-pattern2 context-v params subject remaining []))
  ([context-v params subject remaining output]
    ;(println "process-where-subgrph-pattern2 remaining" remaining)
   (if (empty? remaining)
     output                                                ;is last we finished
     (let [pred (first remaining) obj (second remaining)]
       (recur
         context-v
         params
         subject
         (nthrest remaining 2)
         (conj
           output
           (process-triple-element-to-sparql-element context-v params subject)
           " "
           (process-triple-element-to-sparql-element context-v params pred)
           " "
           (process-triple-element-to-sparql-element context-v params obj)
           ".\n"
           ))))))

(defn process-query-where-statement
  "takes where structure and build list for str. params is set"
  ([query-where context-v params] (process-query-where-statement query-where context-v params []))
  ([query-where context-v params output]
   (let [item (first query-where)
         context-map {:params params :vars-name context-v}]
     (if (or (list? item) (vector? item))
       (let [first-item (first item)]
         (if (keyword? first-item)
           (case (name first-item)
             "filter" (recur (rest query-where) context-v params (concat output (buildfilter context-map (second item))))
             "minus" (recur (rest query-where ) context-v params (concat output " MINUS { " (process-query-where-statement
                                                                                                        (rest item)
                                                                                                        context-v
                                                                                                        params) " } \n"))
             output)
           ; else - not keyword
           (recur (rest query-where)
                  context-v
                  params
                  (concat output (process-where-subgrph-pattern2 context-v params item)))))
         ;else - not list
       output))))

(defn extract-fresh-vars
  ([pattern]  (extract-fresh-vars pattern #{} #{} ) )
  ([query-where params] (extract-fresh-vars query-where params #{}) )
  ([form params fresh-vars]
    ;(println "form:" form)
   (cond
     (coll? form)
     (->>
       (if (coll? (first form))
         (remove #(keyword? (first %)) form)
         form)
       (map #(extract-fresh-vars % params fresh-vars))
       (flatten)
       (apply clojure.set/union fresh-vars))
     (and (r/variable? form) (not (contains? params form )))
      (conj fresh-vars form)
     ;:else nil
     )))


;not sure if this module best place
(defn sparql-var-binding-to-eavariable
  "get uri from rdf and creates variable"
  [var-binding]
  (let [{t :type} var-binding
        {v :value} var-binding]
    (case t
      "uri" (let [parsed-val (re-find #"(?<=\/)[\d\w\-]+$" v) ] ;parse for last words and digits in uri right after last /
              {
               :type      (keyword t)
               :value     parsed-val
               :prefix-ns (subs v 0 (- (count v) (count parsed-val)))})
      "literal"
      {
       :type     (keyword t)
       :datatype (var-binding :datatype)
       :value    (case (var-binding :datatype)
                   "http://www.w3.org/2001/XMLSchema#dateTime"
                   (if (= (last v) \Z)
                     (java.time.ZonedDateTime/parse v)
                     (java.time.LocalDateTime/parse v))
                   v)}
      "typed-literal"                                       ; db-pedia returns that type
      {
       :type     (keyword t)
       :datatype (var-binding :datatype)
       :value    (case (var-binding :datatype)
                   "http://www.w3.org/2001/XMLSchema#dateTime"
                   (if (= (last v) \Z)
                     (java.time.ZonedDateTime/parse v)
                     (java.time.LocalDateTime/parse v))
                   v)})))


(defn append-sparql-result-to-context
  "get sparql results (vector of bindings/maps) and creates context (for ea)"
  ([sparql-result map-to-append]
    ;(println sparql-result)
   (append-sparql-result-to-context
     sparql-result
     (keys sparql-result)
     map-to-append))
  ([sparql-result ks output]
   (if-let [k (first ks)]
     (recur
       sparql-result
       (rest ks)
       (assoc
         output
         (keyword (str "?" (name k))) ; we add ? to variables
         (sparql-var-binding-to-eavariable (sparql-result k))))
     output)))

; currently we build 2 sraql strings - one for ASK another for SELECT
(defn build-event [namespaces bgp]
  ; (println (name (first bgp)))
  {:ask (str "ASK {"
        (clojure.string/join
          " "
          (map
            #(if (r/variable? %) (name %) (r/keyword-to-rdf namespaces %))
            bgp))
        "} ")
   :select (str
             "SELECT \n"
             (clojure.string/join " " (extract-fresh-vars bgp))
             "\n WHERE {\n"
             (clojure.string/join
               " "
               (map
                 #(if (r/variable? %) (name %) (r/keyword-to-rdf namespaces %))
                 bgp))
             "}\n" )})


(defn namespaces-prefixes-map-to-spaqrl
  [namspcs-prefxs-map]
  (apply str (map #(str "PREFIX " (name %) ":" " <" (namspcs-prefxs-map %) "> \n") (keys namspcs-prefxs-map)))
  )

(defn parse-ordering-part [forms]
  (case (first forms)
    :asc (str " ASC (" (second forms) ") " (parse-ordering-part (nthnext forms 2)))
    :desc (str " DESC (" (second forms) ") " (parse-ordering-part (nthnext forms 2)))
    nil ""
    (str " ASC (" (first forms) ") " (parse-ordering-part (nthnext forms 2)))))


(defn buid-order-by-part [query-where]
  ; (println query-where)
  (if (= (not (nil? query-where)) (seq? query-where) (not (empty? query-where)))
    (let [item (first query-where)]
      (if (and (keyword? item) (= item :order-by))
        (str "ORDERBY " (parse-ordering-part (second query-where)))
        (recur (rest query-where))))))

(defn build-ask-notexists-query
  [namespaces params query-where]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (str
         (namespaces-prefixes-map-to-spaqrl ~namespaces)
         "ASK \n"
         "\n WHERE {\n"
         ~@(process-query-where-statement query-where context-v params)
         "}\n " ))))

(defn build-pre-query
  [namespaces params query-where]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (str
         (namespaces-prefixes-map-to-spaqrl ~namespaces)
         "SELECT \n"
         ~(clojure.string/join " " (extract-fresh-vars query-where params))
         "\n WHERE {\n"
         ~@(process-query-where-statement query-where context-v params)
         "}\n"
         ~@(buid-order-by-part query-where)
         "LIMIT 1"))) )


(defn build-insert
  [namespaces params insert-part query-where context-v]
  `(str
    (namespaces-prefixes-map-to-spaqrl ~namespaces)
    "insert { \n"
    ; create insert triples. The same function that used for where part works well.
    ~@(process-query-where-statement insert-part context-v params)
    "\n} WHERE {\n"
    ~@(process-query-where-statement query-where context-v params)
    "}\n"
    ) )

(defn build-delete
  [namespaces params data context-v]
  `(str
     (namespaces-prefixes-map-to-spaqrl ~namespaces)
     "DELETE DATA { \n"
     ; create insert triples. The same function that used for where part works well.
     ~@(process-query-where-statement data context-v params)
     "\n}"
     ) )

(defmacro build-precondition
  [namespaces params query-where]
  (if query-where
    (if (= (first query-where) :not-exists)
     `{:not-exists ~(build-ask-notexists-query namespaces params (rest query-where))}
     `{:query ~(build-pre-query namespaces params query-where)})))



(defn context-var-to-simple-vector [bindng]
  (if (and (map? bindng) (bindng :type))
    (case (bindng :type)
     :uri (vector (bindng :prefix-ns) (bindng :value))
     :literal (.toString (bindng :value))
     bindng)
    bindng  ))

(defn simplify-context-for-print [context-map]
  (reduce #(assoc %1 %2 (context-var-to-simple-vector (context-map %2))) {} (keys context-map)))

