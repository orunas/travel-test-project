(ns test-project.sparql
  (:require [test-project.core :as c] [test-project.rdf :as r] :reload))

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
          (map #(insert-context-to-inner-expression-and-apply context %) (rest items))
          )
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
     (and (c/variable? item) (contains? params item))
     (list `var-val vars-name (keyword item))
     :else
     item)))


(defn wrap-with-rdf-output-and-replace-vars
  [{:keys [params vars-name]} item]
  (if (or (coll? item) (and (c/variable? item) (contains? params item) ))
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
      ")"))
  )
(defmacro m-and
  [context & items]
  `(f-and ~context ~@items)
 )


(defn f<
  [context item1 item2]
  ;(println "f<" item1)
  `(str
     "("
     ~(wrap-with-rdf-output-and-replace-vars context item1)
     "<"
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
; old code ************************
(defn var-val [context var]
  (let [var-c (context var)]
    (if var-c
      (var-c :value))
    ))
(defn var-out [context var]
  "we expected var as keyword like :?campaign"
  (let [var-ctx (context var)]
    (case (var-ctx :type)
      :uri (str "<" (var-ctx :prefix-ns) (var-ctx :value) ">")
      ;not implemented for literals
      )))

(defn process-triple-element-to-sparql-element
  "triple element to sparql string element"
  [context-v params element]
  (if (and (c/variable? element) (contains? params element))
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
  "takes where structure and build list for str. params is set "
  ([query-where context-v params] (process-query-where-statement query-where context-v params []))
  ([query-where context-v params output]
   (let [item (first query-where)
         context-map {:params params :vars-name context-v}]
     ;(println item)
     (cond
       (or (list? item) (vector? item)) (recur
                                          (rest query-where)
                                          context-v
                                          params
                                          (concat output (process-where-subgrph-pattern2 context-v params item)))
       (keyword? item)
       (if (= (name item) "filter")
         ;(str output  (process-query-filter context (second items) (str " FILTER (") )" )" )
         (concat output (buildfilter context-map (second query-where)))
         output
         )))))

(defn extract-fresh-vars
  ([pattern]  (extract-fresh-vars pattern #{} #{} ) )
  ([query-where params] (extract-fresh-vars query-where params #{}) )
  ([form params fresh-vars]
   (cond
     (coll? form)
      (apply clojure.set/union fresh-vars (flatten (map #(extract-fresh-vars % params fresh-vars) form)))
     (and (c/variable? form) (not (contains? params form )))
      (conj fresh-vars form)
     ;:else nil
     ))
  )
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

(defmacro build-pre-query
  [namespaces params query-where ]
  (let [context-v (gensym "vars-")]
    `(fn [~context-v]
       (str
         (c/namespaces-prefixes-map-to-spaqrl ~namespaces)
         "SELECT \n"
         ~(clojure.string/join " " (extract-fresh-vars query-where params) )
         "\n WHERE {\n"
         ~@(process-query-where-statement query-where context-v params)
         "}\n"
         )
       )
    )
  )