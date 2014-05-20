(ns main.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.browser.repl :as repl]))

(enable-console-print!)

(js/Trello.authorize (clj->js {:name "Time Treveller"
                               :expiration "never"}))

(defn trello [method args path callback]
  (js/Trello.rest method
                  path
                  (clj->js args)
                  (fn [result status jqxhr]
                    (callback (js->clj result :keywordize-keys true)))
                  (fn [a]
                    (js/console.log (str "failed " a)))))

(def app-state (atom {}))

(defn div [props & children]
  (apply dom/div (clj->js props) children))

(defn card-component [card owner]
  (om/component
   (div {:className "card"
         :style {:flex "none"}}
        (:name card))))

(defn list-component [list owner]
  (om/component
   (div {:className "list"
         :style {:width 300
                 :flex "none"
                 :display "flex"
                 :max-height "100%"
                 :flex-direction "column"}}
        (div {:style {:flex "none"
                      :padding "8px 8px 0 8px"}}
             (:name list))
        (apply div {:style {:overflow-y "auto"}}
               (om/build-all card-component (:cards list))))))

(defn board-component [board owner]
  (om/component
   (div {:className "board"}
        (div {:style {:height 30
                      :line-height 30}}
             (:name board))
        (apply div {:style {:display "flex"
                            :flex-direction "row"
                            :flex "1 0 auto"
                            :align-items "flex-start"
                            :overflow-x "auto"
                            :position "absolute"
                            :top 50
                            :bottom 0
                            :left 0
                            :right 0
                            :padding 8
                            :background-color "#f8f"}}
               (om/build-all list-component (:lists board))))))

(defn app-component [app-state owner]
  (om/component
   (let [on-change (fn [e]
                     (js/console.log e)
                     (println (-> e :target :value)))]
     (div nil
          (dom/input #js {:type "range"
                          :min 0
                          :max 10
                          :onChange on-change
                          :style #js {:display "block"
                                      :height 20
                                      :width "100%"}})
          (om/build board-component (:board app-state))))))

(om/root app-component
         app-state
         {:target (js/document.getElementById "my-app")})

(defn sanitize-board [board]
  (let [cards (:cards board)
        card-map (group-by :idList cards)
        sanitize-list (fn [list] (assoc list :cards (get card-map (:id list))))]
    (assoc board :lists (map sanitize-list (:lists board)))))

(trello "GET"
        {:lists "open"
         :list_fields "name"
         :cards "open"
         :card_fields "name,idList"}
        "boards/2NRtSl8O"
        #(swap! app-state assoc :board (sanitize-board %)))
