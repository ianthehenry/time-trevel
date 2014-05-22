(ns main.core
  (:require
   [clojure.string :as string]
   [om.core :as om :include-macros true]
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

(def app-state (atom {:latest-board {} :actions []}))

;;;

(defn vec->id-map [v]
  (->> v
       (map #(-> [(% :id) %]))
       (flatten)
       (apply hash-map)))

(defn sanitize-card [card]
  (update-in card [:idMembers] set))

(defn sanitize-board [board]
  (let [cards (->> board :cards (map sanitize-card) vec->id-map)
        lists (->> board :lists vec->id-map)]
    (-> board
        (merge {:cards cards
                :lists lists})
        (dissoc :actions :members))))

;;;

(defn card-id [action] (-> action :data :card :id))

(defmulti rewind-action (fn [board {:keys [type]}] type))

(defn apply-card-action [board action f]
  (update-in board [:cards (card-id action)] f))

(defmethod rewind-action "updateCard" [board action]
  (apply-card-action board action
                     (fn [card]
                       (when (nil? card)
                         (println "updateCard action for unknown card!"))
                       ; we assume in other rewinders that
                       ; idMembers is always a set. (because
                       ; (conj nil :foo) gives (list :foo))
                       (merge (or card {:idMembers #{}})
                              (-> action :data :old)))))

(defmethod rewind-action "addMemberToCard" [board action]
  (apply-card-action board action
                     (fn [card]
                       (update-in card [:idMembers] #(disj % (-> action :data :idMember))))))

(defmethod rewind-action "removeMemberFromCard" [board action]
  (apply-card-action board action
                     (fn [card]
                       (update-in card [:idMembers] #(conj % (-> action :data :idMember))))))

(defmethod rewind-action "updateBoard" [board action]
  (merge board (-> action :data :old)))

(defmethod rewind-action "updateList" [board action]
  (update-in board [:lists (-> action :data :list :id)]
             #(merge % (-> action :data :old))))

(doseq [type ["createCard"
              "convertToCardFromCheckItem"
              "copyCard"
              "moveCardFromBoard"]]
  (defmethod rewind-action type [board action]
    (dissoc-in board [:cards (card-id action)])))

(doseq [type ["deleteCard"
              "moveCardToBoard"]]
  (defmethod rewind-action type [board action]
    (assoc-in board [:cards (card-id action)] (-> action :data :card sanitize-card))))

(doseq [type ["moveListToBoard"]]
  (defmethod rewind-action type [board action]
    (assoc-in board [:lists (-> action :data :list :id)] (-> action :data :list))))

(doseq [type ["createList"
              "moveListFromBoard"]]
  (defmethod rewind-action type [board action]
    (dissoc-in board [:lists (-> action :data :list :id)])))

(def no-op-actions #{"updateCheckItemStateOnCard"
                     "addChecklistToCard"
                     "commentCard"
                     "copyCommentCard"
                     "makeAdminOfBoard"
                     "unconfirmedBoardInvitation"
                     "addMemberToBoard"
                     "addAttachmentToCard"
                     "deleteAttachmentFromCard"
                     "makeNormalMemberOfBoard"})

(defmethod rewind-action :default [board action]
  (println (str "unknown action type " (action :type) (str " with data: " (pr-str action))))
  board)

;;;

(defn div [props & children]
  (apply dom/div (clj->js props) children))

(defn member-component [member owner]
  (om/component
   (div {:className "member"}
        (if (member :avatarHash)
          (let [img-src (str "https://trello-avatars.s3.amazonaws.com/" (member :avatarHash) "/30.png")]
            (dom/img (clj->js {:src img-src})))
          (member :initials)))))

(defn card-component [card owner]
  (om/component
   (let [members (card :members)]
     (div {:className (if (empty? members) "card" "card with-members")}
          (div nil (card :name))
          (apply div {:className "member-container"}
                 (om/build-all member-component members))))))

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
                  (list :name))
             (apply div {:style {:overflow-y "auto"
                                 :flex "0 1 auto"}}
                    (om/build-all card-component (list :cards)))))))

(defn board-component [[board member-map] owner]
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
             (board :name))
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
                     resolve-members (fn [card]
                                       (-> card
                                           (assoc :members (->> (card :idMembers)
                                                                (map member-map)
                                                                (sort-by :id)))
                                           (dissoc :idMembers)))
                     card-map (->> cards
                                   (remove :closed)
                                   (map resolve-members)
                                   (group-by :idList))
                     list-with-cards (fn [list]
                                       (assoc list :cards (->> list
                                                               :id
                                                               card-map
                                                               (sort-by :pos))))
                     friendly-lists (->> lists
                                         (remove :closed)
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
                       (om/build slider-component {:actions (app-state :actions)
                                                   :change-handler #(om/set-state! owner :time-index %)
                                                   :time-index time-index})
                       (let [actions (take time-index (app-state :actions))
                             board (reduce rewind-action (app-state :latest-board) actions)]
                         (om/build board-component [board (app-state :members)]))))))

(om/root app-component
         app-state
         {:target (js/document.getElementById "my-app")})

(trello "GET"
        {:lists "all"
         :list_fields "name,pos,closed"
         :cards "all"
         :card_fields "name,idList,pos,idMembers,closed"
         :members "all"
         :member_fields "avatarHash,initials"
         :actions "all"
         :actions_limit 1000}
        "boards/dbdNNMrI"
        (fn [board]
          (reset! app-state {:actions (->> board :actions (remove #(contains? no-op-actions (% :type))))
                             :members (->> board :members vec->id-map)
                             :latest-board (sanitize-board board)})))
