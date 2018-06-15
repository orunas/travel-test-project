(ns test-project.ws
  (:require [org.httpkit.client :as http]))

(defn GetWS [url params-map]
  ;(println "url:" url)
  ;(println "params-map:" params-map)
  (let [{:keys [status headers body error] :as resp} @(http/get url params-map)]
    (if (= status 200)
      body
      (do
        (println "url" url " status:" status " error:" error " body:" body " params:" params-map)
        :error-action) ))
  )

(defn Post2WS
  [url params-map]
  (let [{:keys [status headers body error] :as resp} @(http/post url params-map)]
    (if (or (= status 200) (= status 204))
      body
      (do
        (println "url" url " status:" status " error:" error " body:" body " params:" params-map)
        :error-action) )))

(defn CallWS
  ([url body headers]
   (def options {:headers headers :body body})
    ;(println "sending data:\n" body " to url:\n" url "\n")
   (Post2WS url options)))




