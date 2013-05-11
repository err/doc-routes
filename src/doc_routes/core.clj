(ns doc-routes.core
  (:use [clojure.tools.macro :only (name-with-attributes)]
        [compojure.core]
        [hiccup.core]
        [clojure.pprint :only (pprint)])
  (:require [net.cgrand.enlive-html :as enlive]))

;; Need ../resources, since the root is ./src
(enlive/deftemplate doc-page "../resources/template.html"
  [route-map]
  [:pre#route] (enlive/content (str (:method route-map) " "
                                    (:route route-map))))

(defn make-doc-page
  "doc me"
  [route-maps]
  ;; (let [{:keys [method route params doc body]} route-map])
  (prn [:route-map route-maps])

  ;; For testing, just use the first item in the map to create some basic 
  ;; doc pages, then work on creating doc pages w/ multiple routes
  (let [route-map (nth route-maps 0)
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
;;(doc-routes app 
;;            (GET "/foo/bar" [id foo bar] {:body "Foobar!"})
;;            (GET "/foo/baz" [id foo baz] {:body "Foobaz!"})
;;            (POST "/foo/baz" [id foo baz] {:body "Foobaz!"}))


;;; docstring parsing 
;; :args (re-find #"(?s) \(args[^\)]*\)" docstr)
;; :curl (re-find #"(?s) \(curl[^\)]*\)" docstr)
;; :test (re-find #"(?s) \(test[^\)]*\)" docstr)
