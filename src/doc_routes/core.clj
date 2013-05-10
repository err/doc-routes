(ns docroutes
  (:use [clojure.tools.macro :only (name-with-attributes)]
        [compojure.core]
        [hiccup.core]))

(defn make-doc-page
  "doc me"
  [route-maps]
  ;; (let [{:keys [method route params doc body]} route-map])
  (prn [:route-map route-maps])
  ;; (html [:h1 route]
  ;;       [:h2 "Params: " params]
  ;;       [:p doc]
  ;;       [:br]
  ;;       [:h3 "src"]
  ;;       [:p body])
  :foo
  )

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


(defmacro defroutes
  "Define a Ring handler function from a sequence of routes. The name may
  optionally be followed by a doc-string and metadata map."
  [name & routes]
  (let [[name routes] (name-with-attributes name routes)]
   `(def ~name (routes ~@routes))))



;;; docstring parsing 
;; :args (re-find #"(?s) \(args[^\)]*\)" docstr)
;; :curl (re-find #"(?s) \(curl[^\)]*\)" docstr)
;; :test (re-find #"(?s) \(test[^\)]*\)" docstr)
