(ns test-project.core
  (:require [org.httpkit.client :as http]
            )
  )


;(c/parse-body-for-vars (first (second (t/update-data-on-campaign :body))) (first (c/make-agenda t/plans (c/main t/plans))))
(comment
  (require '[test-project.core :as c] '[test-project.rdf :as r] '[test-project.test-plan :as t] '[test-project.match  :as u] '[test-project.sparql :as s] '[test-project.test-plan2 :as t2] '[test-project.ea :as e])
  (r/defns t "http://travelplanning.ex/")
  (r/defns xsd "http://www.w3.org/2001/XMLSchema#"))

(defn test-time
  []
  (let [started (java.time.LocalDateTime/now)]
    (println started (java.time.LocalDateTime/now)
             )
    ))

; name.precondition() name.body()
; defplan creates function that gets parameters variables and returns initialized plan with parameters and fresh variables
; plan body returns list of functions
; to progress plan algorithm should take

;{:remote-addr 0:0:0:0:0:0:0:1, :headers {host localhost:8087, user-agent PostmanRuntime/7.1.1, content-type text/turtle; charset=utf-8, content-length 2031, connection keep-alive, accept */*, accept-encoding gzip, deflate, postman-token e70660f2-c617-4b6c-8358-41ffa61df4e2, cache-control no-cache}, :async-channel #object[org.httpkit.server.AsyncChannel 0x49ad6eb /0:0:0:0:0:0:0:1:8087<->/0:0:0:0:0:0:0:1:9594], :server-port 8087, :content-length 2031, :websocket? false, :content-type text/turtle, :character-encoding utf-8, :uri /1, :server-name localhost, :query-string nil, :body #object[org.httpkit.BytesInputStream 0x5a8dbbd1 BytesInputStream[len=2031]], :scheme :http, :request-method :post}
