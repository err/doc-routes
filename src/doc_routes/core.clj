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
(enlive/defsnippet param-row "docroutes_template_param_row.html" [:#param-row]
  [[param-name param-desc required :as param]]
  [:#param-name] (enlive/content (if required
                                   (str param-name "*")
                                   (str param-name)))

  ;; TODO - rather than using bootstrap "text-error" css class in the case of
  ;; a required param, we should probably either find a better suited bootstrap
  ;; class, or write our own.
  [:#param-name] (enlive/add-class (if required "text-error" "muted"))
  [:#param-desc] (enlive/content (str param-desc)))

(enlive/defsnippet verb-doc "docroutes_template_route_verb.html" [:#verb-snippet]
  [{:keys [method route response-body response-status doc]}]
  [:#verb-anchor] (enlive/set-attr :name method)
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
                                        (apply str))))

(enlive/deftemplate doc-page "docroutes_template_route.html"
  [route-maps]
  [:#header-col] (enlive/html-content
                  (str "<ul>"
                       (apply str
                              (map #(str "<li>"
                                            "<a href=\"#" (:method %) "\" >"
                                               (:method %)
                                            "</a>"
                                         "</li>")
                                   route-maps))
                       "</ul>"))
  [:#verb-col] (enlive/html-content (->> route-maps
                                         (map verb-doc)
                                         (map enlive/emit*)
                                         flatten
                                         (apply str))))

(defn make-doc-page
  "doc me"
  [route-maps]
  ;; (let [{:keys [method route params doc body]} route-map])
  (prn [:route-map route-maps])

  (let [;;; cURLing
        ;; Will fail unless you've got the route from the sample.
        ;; test-url (str "http://localhost:2000" (:route route-map) "?"
        ;;               (-> route-map :doc :curl))
        ;; test-response "" ; TODO: working on incorporation of this for next phase... (client/get test-url)
        ;; route-map (merge route-map {:response-body (:body test-response)
        ;;                             :response-status (:status
        ;;                             test-response)})
        _ (prn (first route-maps))
        route-name (subs (:route (first route-maps)) 1)
        filename (.replace route-name "/" "_")
        filename (str "doc/docroutes/" filename ".html")
        doc-page-str (reduce str (doc-page route-maps))]
    ;; (prn "filename=" filename)
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
; (defn -main
;   [& args]
;   (doc-routes app
;               (GET "/v1/reviews"
;                    [product-id date limit offset access-key-id]
;                    {:service-name "Get Reviews"
;                     :does "Gets reviews of a property/listing."
;                     :args [["property-id" "the stable property/listing id"
;                             :required]
;                            ["date"        "if specified, reviews within
;                                           one day of the start date are
;                                           returned (MMddyy format)"]
;                            ["limit"       "the number of reviews to return,
;                                           defaults to 1000"]
;                            ["offset"      "indicates which review should be used
;                                           as the start"]
;                            ["access-key-id"  "your public api key assigned by Rentpath"
;                             :required]
;                            ["signature"   "the hmac signature of your request"
;                             :required]]
;                     :request-body ""
;                     :curl  "property-id=999&date=01012012&access-key-id=test&signature=test"}
;                    {:body "Foobar!"})))


;;; docstring parsing
;; :args (re-find #"(?s) \(args[^\)]*\)" docstr)
;; :curl (re-find #"(?s) \(curl[^\)]*\)" docstr)
;; :test (re-find #"(?s) \(test[^\)]*\)" docstr)
