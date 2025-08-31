(ns org.goat.http.urlserver
  (:require [org.goat.db.urls :as urls]
            [org.httpkit.server :as server]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]))

(defroutes url-routes
           (GET "/urls/" [] (response (urls/get-urls 200)))
           (GET "/urls/:search" [search] (response (urls/get-urls 200 0 (str "%" search "%"))))
           (route/not-found {:error "Not found" :status "error"}))

;; middleware stack
(def app
  (-> url-routes
      wrap-json-response
      (wrap-json-body {:keywords? true})))

;;server mgmt
(defonce server (atom nil))

(defn start-server [port]
  (reset! server
    (server/run-server app {:port port :legacy-return-value? false}))
  (println (str "Server started on port " port)))

(defn stop-server []
  (server/server-stop! @server)
  (reset! server nil)
  (println "Server stopped"))

(comment
  (start-server 60000)

  (stop-server)

  (urls/get-urls 200)

  (urls/get-urls 200 0 (str "%" "c" "%"))
)

;(start-server 60000)
