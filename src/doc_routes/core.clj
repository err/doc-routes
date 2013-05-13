(ns doc-routes.core
  (:use [clojure.tools.macro :only (name-with-attributes)]
        [compojure.core]
        [hiccup.core]
        [clojure.pprint :only (pprint)])
  (:require [net.cgrand.enlive-html :as enlive]
            [clj-http.client :as client]))

;; For template/snippets we need ../resources, since the root is ./src

;; TODO - using ids as selectors isn't a good idea for rows that we're going
;; to duplicate in the final html.
;; TODO - host for curl should be externalized.
(enlive/defsnippet param-row "../resources/template_param_row.html" [:#param-row]
  [[param-name param-desc :as param]]
  [:#param-name] (enlive/content (str param-name))
  [:#param-desc] (enlive/content (str param-desc)))

(enlive/deftemplate doc-page "../resources/template.html"
  [{:keys [method route response-body response-status doc]}]
  [:#route] (enlive/content (str method " " route))
  [:#service-name] (enlive/content (:service-name doc))
  [:#service-desc] (enlive/content (:does doc))
  [:#request-body] (enlive/content (:request-body doc))
  [:#response-body] (enlive/content response-body)
  [:#response-status] (enlive/content (str response-status))
  [:#curl] (enlive/content (str "curl http://<host>" route "?" (:curl doc)))
  [:#param-rows] (enlive/html-content (->>
                                        doc
                                        :args
                                        seq
                                        (map param-row)
                                        (map enlive/emit*)
                                        flatten
                                        (reduce str))))


(defn make-doc-page
  "doc me"
  [route-maps]
  ;; (let [{:keys [method route params doc body]} route-map])
  (prn [:route-map route-maps])

  (let [;; For testing, just use the first item in the map to create some basic 
        ;; doc pages, then work on creating doc pages w/ multiple routes
        route-map (nth route-maps 0)

        ;; Will fail unless you've got the route from the sample.
        ;test-url (str "http://localhost:2000" (:route route-map) "?" 
        ;              (-> route-map :doc :curl))
        ;test-response (client/get test-url)
        ;route-map (merge route-map {:response-body (:body test-response)
        ;                            :response-status (:status test-response)})

        filename (.replace (:route route-map) "/" "-")
        filename (str "output/" filename ".html")
        doc-page-str (reduce str (doc-page route-map))]
    (spit filename doc-page-str))

  :foo)

(defn document-routes [route-forms]
  (let [routes (for [[method route params doc & body :as form] route-forms
                     :let [has-doc? (and (seq body) ((some-fn string? map?) doc))
                           docstring (if has-doc? doc "")
                           response  (if has-doc? body (list doc))]]
                 {:method  method
                  :route   route
                  :params  params
                  :doc     docstring
                  :body    response})]

    (->> routes

         ;; build structure
         (reduce (fn [res {:keys [method route params doc body]}]
                   (update-in res [route (name method)]
                              merge {:method (name method)
                                     :route route
                                     :params params
                                     :doc  doc
                                     :body body}))
                 {})

         ;; build docpage
         (reduce (fn [res [r m :as route]]
                   ;; (prn "RES  : " res)
                   ;; (prn "RMAP : " route)
                   (let [path (str r "/doc")]
                     (-> res
                         (assoc-in [r] m)
                         (assoc-in [path "GET"]
                                   {:method "GET"
                                    :route  path
                                    :params []
                                    :doc  (str "doc page for " r)
                                    :body (list (make-doc-page (vals m)))}))))
                 {})

         #_pprint

         ;;  new routes
         (mapcat (fn [[r m]]
                   (let [methods (keys m)]
                     (map (fn [meth]
                            (let [route (get m meth)]
                              (concat (list (symbol meth) r (:params route))
                                      (:body route))))
                          methods)))))))


(defmacro doc-routes
  [name & routes]
  (let [[name routes] (name-with-attributes name routes)
        routes (document-routes routes)]
    (pprint routes)
   `(def ~name (routes ~@routes))))


;; Sample -- include a few globals, not sure where these will come from.
(defn -main
  [& args]
  (doc-routes app 
              (GET "/v1/reviews" 
                   [product-id date limit offset access-key] 
                   {:service-name "Get Reviews"
                    :does "Gets all the reviews of a property/listing."
                    :args {"property-id" "the stable property/listing id"
                           "date"        "if specified, reviews within
                                         one day of the start date are
                                         returned (MMddyy format)"
                           "limit"       "the number of reviews to return, 
                                         defaults to 1000"
                           "offset"      "only reviews after the offset are
                                         returned"
                           "access-key"  "your api key assigned by Rentpath"}
                    :request-body ""
                    :curl  "property-id=999&date=01012012&access-key=test"}
                   {:body "Foobar!"})))


;;; docstring parsing 
;; :args (re-find #"(?s) \(args[^\)]*\)" docstr)
;; :curl (re-find #"(?s) \(curl[^\)]*\)" docstr)
;; :test (re-find #"(?s) \(test[^\)]*\)" docstr)
