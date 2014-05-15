(ns main.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.browser.repl :as repl]))

(enable-console-print!)

(def app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))


(defn board-widget [board owner]
  (reify
    om/IRender
    (render [this]
            (apply dom/ul nil
                   (let [card-names (map :name (:cards board))]
                     (map (partial dom/li nil) card-names))))))

(om/root board-widget
         app-state
         {:target (js/document.getElementById "my-app")
          :path [:board]})


(js/Trello.authorize (clj->js {:type "popup"
                               :name "Time Treveller"
                               :expiration "never"}))

(defn trello [method args path]
  (js/Trello.rest method
                  path
                  (clj->js args)
                  (fn [a & b]
                    (swap! app-state assoc :board (js->clj a :keywordize-keys true)))
                  (fn [a]
                    (js/console.log "failed"))))

(trello "GET" {:cards "open"} "boards/2NRtSl8O")

; (println (-> @app-state :board :cards))

