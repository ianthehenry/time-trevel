(ns main.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

; TODO: figure out how to include the clojure core incubator as a dependency
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


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

;;;

(defn card-id [action] (-> action :data :card :id))

(defmulti rewind-action (fn [board {:keys [type]}] type))

(defmethod rewind-action "updateCard" [board action]
  (update-in board [:cards (card-id action)]
             #(merge % (-> action :data :old))))

(defmethod rewind-action "updateList" [board action]
  (update-in board [:lists (-> action :data :list :id)]
             #(merge % (-> action :data :old))))

(doseq [type ["createCard" "convertToCardFromCheckItem"]]
  (defmethod rewind-action type [board action]
    (dissoc-in board [:cards (card-id action)])))


(defmethod rewind-action :default [board action]
  (println (str "unknown action type " (:type action) " with data: " (pr-str action)))
  board)

;;;

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
                 :white-space "normal"
                 :vertical-align "top"
                 :display "inline-block"
                 :max-height "100%"}}
        (div {:style {:display "flex"
                      :max-height "100%"
                      :flex-direction "column"}}
             (div {:style {:flex "none"
                           :padding "8px 8px 0 8px"}}
                  (:name list))
             (apply div {:style {:overflow-y "auto"
                                 :flex "0 1 auto"}}
                    (om/build-all card-component (:cards list)))))))

(defn board-component [board owner]
  (om/component
   (div {:className "board"
         :style {:position "absolute"
                 :top 80
                 :bottom 0
                 :left 0
                 :right 0
                 :background-color "rgb(35, 113, 159)"}}
        (div {:style {:height 30
                      :line-height 30
                      :font-size 18
                      :padding "0 8px"
                      :color "white"}}
             (:name board))
        (apply div {:style {:white-space "nowrap"
                            :overflow-x "auto"
                            :position "absolute"
                            :top 30
                            :bottom 0
                            :left 0
                            :right 0
                            :padding "8"}}
               (let [cards (-> board :cards vals)
                     lists (-> board :lists vals)
                     card-map (group-by :idList cards)
                     list-with-cards (fn [list]
                                       (assoc list :cards (->> list
                                                               :id
                                                               card-map
                                                               (sort-by :pos))))
                     friendly-lists (->> lists
                                         (map list-with-cards)
                                         (sort-by :pos))]
                 (om/build-all list-component friendly-lists))))))

(defn slider-component [{:keys [actions change-handler time-index]} _]
  (om/component
   (div nil (div nil (str time-index))
   (dom/input #js {:type "range"
                   :min 0
                   :max (count actions)
                   :value time-index
                   :onChange #(change-handler (.. % -target -value))
                   :style #js {:display "block"
                               :height 20
                               :margin "0 auto"
                               :width "90%"}}))))

(defn app-component [app-state owner]
  (reify
    om/IInitState
    (init-state [_] {:time-index 0})
    om/IRenderState
    (render-state [_ {:keys [time-index]}]
      (div nil
           (om/build slider-component {:actions (-> app-state :board :actions)
                                       :change-handler #(om/set-state! owner :time-index %)
                                       :time-index time-index})
           (let [board (:board app-state)
                 all-actions (:actions board)
                 actions (take time-index all-actions)
                 past-board (reduce rewind-action board actions)]
             (om/build board-component past-board))))))

(om/root app-component
         app-state
         {:target (js/document.getElementById "my-app")})

(defn sanitize-board [board]
  (let [id-list-to-map (fn [list-of-models]
                         (->> list-of-models
                              (map #(-> [(:id %) %]))
                              (flatten)
                              (apply hash-map)))
        cards (id-list-to-map (:cards board))
        lists (id-list-to-map (:lists board))]
    (merge board {:cards cards
                  :lists lists})))

(trello "GET"
        {:lists "open"
         :list_fields "name,pos"
         :cards "open"
         :card_fields "name,idList,pos"
         :actions "all"
         :actions_limit 10}
        "boards/2NRtSl8O"
        #(swap! app-state assoc :board (sanitize-board %)))
