(ns doc-routes.core
  (:use [clojure.tools.macro :only (name-with-attributes)]
        [compojure.core]
        [hiccup.core]
        [clojure.pprint :only (pprint)])
  (:require [net.cgrand.enlive-html :as enlive]))

;; For template/snippets we need ../resources, since the root is ./src

;; TODO - using ids as selectors isn't a good idea for rows that we're going
;; to duplicate in the final html.
(enlive/defsnippet param-row "../resources/template_param_row.html" [:#param-row]
  [param]
  [:#param-name] (enlive/content param)
  [:#param-desc] (enlive/content (str param " description")))

(enlive/deftemplate doc-page "../resources/template.html"
  [route-map]
  [:#route] (enlive/content (str (:method route-map) " "
                                    (:route route-map)))
  [:#service-name] (enlive/content (:service-name route-map))
  [:#service-desc] (enlive/content (:service-desc route-map))
  [:#param-rows] (enlive/html-content (->>
                                        route-map
                                        :params
                                        (map str)
                                        (map param-row)
                                        (map enlive/emit*)
                                        flatten
                                        (reduce str))))

(defn make-doc-page
  "doc me"
  [route-maps]
  ;; (let [{:keys [method route params doc body]} route-map])
  (prn [:route-map route-maps])

  (let [;; A few properties we need to capture, but aren't sure how to get yet,
        ;; so just mocking them up.
        props-we-need {:service-name "Get Reviews"
                       :service-desc "Get the reviews of a property."}
        ;; For testing, just use the first item in the map to create some basic 
        ;; doc pages, then work on creating doc pages w/ multiple routes
        route-map (merge (nth route-maps 0) props-we-need)
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


;; Sample
(defn -main
  [& args]
  (doc-routes app 
              (GET "/foo/bar" [id foo bar] {:body "Foobar!"})
              (GET "/foo/baz" [id foo baz] {:body "Foobaz!"})
              (POST "/foo/baz" [id foo baz] {:body "Foobaz!"})))


;;; docstring parsing 
;; :args (re-find #"(?s) \(args[^\)]*\)" docstr)
;; :curl (re-find #"(?s) \(curl[^\)]*\)" docstr)
;; :test (re-find #"(?s) \(test[^\)]*\)" docstr)
