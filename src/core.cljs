(ns main.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.browser.repl :as repl]))

(enable-console-print!)

(js/Trello.authorize (clj->js {:type "popup"
                               :name "Time Treveller"
                               :expiration "never"}))

(defn trello [method args path callback]
  (js/Trello.rest method
                  path
                  (clj->js args)
                  (fn [result status jqxhr]
                    (callback (js->clj result :keywordize-keys true)))
                  (fn [a]
                    (js/console.log (str "failed " a)))))

;;;

(def app-state (atom {}))


(defn style [dict]
  (clj->js {:style dict}))

(defn card-component [card owner]
  (om/component
   (dom/div (style {:background-color "#fff"
                    :margin 8})
            (:name card))))

(defn list-component [list owner]
  (om/component
   (dom/div (style {:background-color "#bbb"
                    :width 300
                    :flex "none"})
            (dom/h2 nil (:name list))
            (apply dom/div nil
                   (om/build-all card-component (:cards list))))))

(defn board-component [board owner]
  (om/component
   (dom/div nil
            (dom/div nil (:name board))
            (apply dom/div (style {:display "flex"
                                   :flex-direction "row"
                                   :align-items "flex-start"
                                   :justify-content "flex-start"})
                   (om/build-all list-component (:lists board))))))

(om/root board-component
         app-state
         {:target (js/document.getElementById "my-app")
          :path [:board]})

(defn sanitize-board [board]
  (let [cards (:cards board)
        card-map (group-by :idList cards)
        sanitize-list (fn [list] (assoc list :cards (get card-map (:id list))))]
    (assoc board :lists (map sanitize-list (:lists board)))))

(:lists (sanitize-board (:board @app-state)))

(trello "GET"
        {:lists "open"
         :list_fields "name"
         :cards "open"
         :card_fields "name,idList"}
        "boards/2NRtSl8O"
        #(swap! app-state assoc :board (sanitize-board %)))

; (js/console.log (clj->js (-> @app-state :board)))
(let [swap (fn [f] (fn [a b] (f b a)))]
  (-> @app-state :board :lists ((swap map) :name)))

