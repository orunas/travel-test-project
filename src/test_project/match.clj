(ns test-project.match)




(defn not-nil? [x] (not (nil? x)))

(defn atom? [x]
  (or (symbol? x) (string? x) (keyword? x) (nil? x) (= (str (type x)) "class java.time.ZonedDateTime") (empty? x) ))

(defn variable-p [x]
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbol? x) (= (get (str (symbol x)) 0) '\?)))

(defn varp[x] "Originaliame kode naudoja tokia funkcija. Man atrodo ta padengiai variable-p. Taciau pacio kodo nekeiciau o sukuriu funkcija" (variable-p x))
(defn equal[x y] (= x y))
(defn car[x] (first x))
(defn cdr[x] (rest x))
(def t true)
(defn null [x] (nil? x))

(defn match-atom [pattern1 pattern2]
  (or
    (= pattern1 pattern2)
    (variable-p pattern1)
    (variable-p pattern2)))

;(match-atom '() '())

(defn match [pattern1 pattern2]
  (cond
    (or (atom? pattern1) (atom? pattern2)) (match-atom pattern1 pattern2)
    t
      (and
        (match (first pattern1) (first pattern2))
        (match (rest pattern1) (rest pattern2)); - ka daryti jei ateina ()
        )))


(defn get-matches [pattern database]
  (cond
    (null database) '()
    (match pattern (car database)) (cons (car database) (get-matches pattern (cdr database)))
    t (get-matches pattern (cdr database))))


;(match '(a b c) '(? c ?))

;(match-atom 'a '?)

;(atom? '?x)

(defn is-constant-p [item]
  (atom? item))

;(is-constant-p 'x)


(defn occursp [var pattern]
  (cond
    (equal var pattern) t
    (or (varp pattern) (is-constant-p pattern)) nil
    t (or (occursp var (car pattern)) (occursp var (cdr pattern)))))


;tokia forma {:?x 'a :?y 'b}    pvz. kvietimo - (get-binding '?x {:?x 'a :?y 'b})
(defn get-binding-value [var substitution-list] "Nera visai taip kaip lisp - grazinu value ne visai pora"
  (get substitution-list (keyword var)))


(defn add-substitution [var pattern substitution-list]
  (assoc substitution-list (keyword var) pattern ))

(defn match-var [var pattern substitution-list]
  (cond
    (equal var pattern) substitution-list
    t
      (let
        [binding (get-binding-value var substitution-list)]
        (cond
          binding ((resolve 'unification.match/unify) binding pattern substitution-list)  ; idejau resolve - taciau labai priklauso is kur kvieciama funkcija
         ; binding (unify binding pattern substitution-list)
          (occursp var pattern) 'failed
          t (add-substitution var pattern substitution-list)))))



(defn unify [pattern1 pattern2 substitution-list]
  (cond
    (equal substitution-list 'failed)  'failed
    (varp pattern1) (match-var pattern1 pattern2 substitution-list)
    (varp pattern2) (match-var pattern2 pattern1 substitution-list)
    (is-constant-p pattern1)
     (cond
       (equal pattern1 pattern2) substitution-list
       t 'failed)
    (is-constant-p pattern2) 'failed
    t (unify (rest pattern1) (rest pattern2) (unify (first pattern1) (first pattern2) substitution-list))))


;(defn get-binding-value [binding] (cdr binding))
;(unify '(P ?x b) '(P ?y ?y) {})
;(match-var '?x 'a {:?y 'd :?x1 'c})


; panasu kad esame situacijoje kai nemoka backtrakingti auksciau
;(defn unify-db [patterns db-rest substitution-list db] ;db tai list'as pattern2
;    (cond
;      (empty? patterns) substitution-list
;      (empty? db-rest) 'end ; jei pasiekem dunga db - tai kaip ir nieko reiskia
;      :else (let [binding (unify (first patterns) (first db-rest) substitution-list)]
;              (cond
;                (equal binding 'failed) (unify-db patterns (rest db-rest) substitution-list db) ; cia einam i kita DB elementa (cia gali buti or
;                :else (let [binding2 (unify-db (rest patterns) db binding db)]
;
;                        ))))) ; cia eina i kita el. bus and - bet jei cia ivaro ne backtrakina


;(defn unify-db-deep [patterns db-rest substitution-list db]
;      (cond
;        (empty? patterns) substitution-list
;        (empty? db-rest) nil ; jei pasiekem dunga db - tai kaip ir nieko reiskia
;        ))

(defn unify-db-single-pattern [pattern db substitution-list]
  (cond
    (empty? db) nil
    t (let [binding (unify pattern (first db) substitution-list)]
        (cond
            (equal binding 'failed) (unify-db-single-pattern pattern (rest db) substitution-list) ; neisprendziai kaip neprideti failed to rekursini iskvietima padubliuoju
            t
              (concat (unify-db-single-pattern pattern (rest db) substitution-list) (list binding)))
        )))


(defn unify-db [patterns db substitution-list]
    (cond
      (empty? patterns) substitution-list
      t (let [bindings (unify-db-single-pattern (first patterns) db substitution-list)]
          (cond
             (empty? bindings) nil
              t (filter not-nil? (map #(unify-db (rest patterns) db %) bindings ))))))


;(concat '({:x a}) (list nil))

;(unify-db-single-pattern '(P a ?y)  '((P a a) (P b b) (P c a) (P a w) (D b a ) (D b w)) {})

;(unify-db '((P a ?y) (D b ?x) ) '((P c a) (P b b) (P a a) (P a w) (D b a ) (D b w)) {})

;(unify-db '((P ?x ?y) (P b ?x)) '((P a a) (P b b) (P c a)) {})

;(def *db* '((Flight VNO STN) (Flight KUN STN) (Flight VNO BRE) (Flight BRE TFN) (Flight STN TFN)))
;(unification.match/unify-db '((Flight ?y ?x) (Flight ?x TFN)) *db* {})

;(unify '(P ?x ?y) '(P b b) {:?x 'b})







;'(Flight UXX12)
;'(Flight-DepartureAirport UXX12 VNO)
;'(Flight-ArrivalAirport UXX12 STN)
;'(Flight ZZ123)
;'(Flight-DepartureAirport ZZ123 STN)
;'(Flight-DepartureAirport ZZ123 TFS)
;'(Flight ?x) (Flight-DepartureAirport ?x ?x-d) (Flight-ArrivalAirport ?x ?x-a) (Flight ?y) (Flight-DepartureAirport ?y ?x-a) (Flight-ArrivalAirport ?y ?y-a)


