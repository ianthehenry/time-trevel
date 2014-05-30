(ns main.trello)

(js/Trello.authorize (clj->js {:name "Time Treveller"
                               :expiration "never"}))

(defn GET [path args callback]
  (js/Trello.rest "GET"
                  path
                  (clj->js args)
                  (fn [result status jqxhr]
                    (callback (js->clj result :keywordize-keys true)))
                  (fn [a]
                    (js/console.log (str "failed " a)))))
