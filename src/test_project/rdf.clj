(ns test-project.rdf
  (:require [clojure.string :as string]))
;from https://github.com/structureddynamics/clj-turtle

(defn- md5
  "Generate a MD5 hash from an input string"
  [s]
  (apply str
         (map (partial format "%02x")
              (.digest (doto (java.security.MessageDigest/getInstance "MD5")
                         .reset
                         (.update (.getBytes s)))))))

(defn- escape
  "Escape data literal for the Turtle data format."
  [s]
  (-> s
      str
      (string/replace #"\"" "\\\\\"")))


(defn keyword-to-rdf [nspcs k]
  (let [[k1 k2] (clojure.string/split (name k) #":")]
    (str "<" (nspcs (keyword k1)) k2 "> ")))

(defn rdf
  "Generate RDF/Turtle serialized data from a set of triples defined by clj-turtle."
  [& rest]
  (loop [forms rest
         n3 "@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> . \n"]
    (if-not (empty? forms)
      (if (and (get (into [] forms) 3)                      ;cia panasu tikrina ar trippe su rein into - join collections (cia panasu vercia i array. ir ziuri ar yra 4'as
               (vector? (nth forms 3)))                     ;tikrina ar grazintas 4'asis yra vectorius
        (recur (drop 4 forms)
               (apply str n3 (nth forms 0) (nth forms 1) (nth forms 2)" .\n"
                    (->> (nth forms 3)
                         (map #(str (nth forms 0) %1 " .\n"))
                         )))
        (recur (drop 3 forms)
               (apply str n3 (nth forms 0) (nth forms 1) (nth forms 2)" .\n")))
      n3)))

(def turtle "Alias for the (rdf) function" rdf)

(defmacro defns
  "Create a new namespace that can be used to create the clj-turtle triples"
  [prefix namespace]
  (let [entity (gensym "entity-")
        body (gensym "body-")]
    `(defn ~prefix
       [~entity & ~body]
       (str "<" ~namespace (name ~entity) "> " (apply str (rest ~body))))))



(defn variable?
  [v]
  (= (first (str v)) \?))



(defn iri
  "Serialize a URI where you provide the full URI as a string"
  [uri]
  (str "<" uri "> "))

(defn var [v]
  (str v)
  )

(defn a
  "Specify the rdf:type of an entity being described"
  []
  (str " <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> "))

(defn literal
  "[v] String that represents the literal value to create.
   [lang] (optional) Keyword referring to the ISO 639-1 language code
   [type] (optional) Namespace function or string that refers to a
                     datatype to specify for the literal value

   Usages:
     ; specify a simple literal
     (literal \"bob\")

     ; specify the language of the literal
     (literal \"bob\" :lang :en)

     ; if the namespace 'xsd' has been defined
     (literal \"bob\" :type (xsd :string))

     ; if you want to use an abreviated URI you can do this that way
     (literal \"bob\" :type \"xsd:string\")"
  [v & {:keys [lang type]
        :or [lang nil
             type nil]}]
  (apply str
         (str "\"\"\"" (if (keyword? v)
                         (escape (name v))
                         (escape v))"\"\"\"")
         (if lang
           (str "@" (name lang))
           (when type
             (str "^^" (name type))))))

(defn rei
  "Reify a clj-turtle triple"
  [& rest]
  (loop [forms rest
         statements []]
    (if-not (empty? forms)
      (recur (drop 2 forms)
             (into statements [(apply str (nth forms 0) (nth forms 1))]))
      statements)))


(defn gen-id
  [ns entity & keys]
  (iri (str ns entity "/" (string/join "_" (->>
                                             (map string/trim keys)
                                             (map #(string/replace % "<" "" ))
                                             (map #(string/replace % ">" "" ))
                                             (map #(string/replace % ns "" ))
                                             )  ))
       ;(eval (prefix (str entity "/" (string/join "_" keys ))))
       ))

(defn now
  []
  (java.time.ZonedDateTime/now (java.time.ZoneId/of "UTC"))
  )

(defn add-days
  [date  days-to-add]
  (if (not (nil? date)) (.plusDays date days-to-add))
  )

(defn dateTime-to-date
  [v]
  (first (string/split (str v) #"T"))
  )

(defn to-rdftype
  "docstring"
  [v]
  (cond
    (= (type v) java.time.LocalDateTime) (literal v :type "xsd:dateTime")
    (= (type v) java.time.ZonedDateTime) (literal (.format v (java.time.format.DateTimeFormatter/ISO_INSTANT)) :type "xsd:dateTime")
    (= (type v) java.lang.String) (literal v :type "xsd:string")
    :else v ))

(defn dateTime-to-id
  [v]
  (.format v (java.time.format.DateTimeFormatter/ofPattern "yyyyMMddHHmmss"))
  )


(defn process-statement
  [statement]
  (cond
    (seq? statement )   (str statement)                                    ;nepabaigta
    (vector? statement) (str statement)
    (variable? statement) (str statement)
    (string? statement) statement
    (clojure.test/function? statement) (to-rdftype (eval (conj () statement)))
    (symbol? statement) (str (to-rdftype (eval statement)))
    :else (str statement)
    )
  )


(defn filter-s
  [rest]
  (let [statements rest]
    (cond
      (= (first statements) 'and)
      (string/join " && " (map #(str "(" % ") ") ( map #(filter-s %1) (into () (next statements)) )))
      (= (first statements) '<)
      (str (process-statement (nth statements 1)) " < " (process-statement (nth statements 2)))
      (= (first statements) '>)
      (str (process-statement (nth statements 1)) " > " (process-statement (nth statements 2)))
      :else (str statements)
      )
    )
  )