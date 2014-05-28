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

(defn trello [path args callback]
  (js/Trello.rest "GET"
                  path
                  (clj->js args)
                  (fn [result status jqxhr]
                    (callback (js->clj result :keywordize-keys true)))
                  (fn [a]
                    (js/console.log (str "failed " a)))))

(def app-state (atom {:board {} :actions []}))

;;;

(defn vec->id-map [v]
  (->> v
       (map #(-> [(% :id) %]))
       (flatten)
       (apply hash-map)))

(defn sanitize-card [card]
  (-> card
      (update-in [:idMembers] set)
      (update-in [:attachments] vec->id-map)
      (update-in [:labels] #(->> % (map :color) set))))

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

(def empty-card {:idMembers #{}
                 :attachments {}
                 :lables #{}
                 :name "(unknown card)"})

(defmethod rewind-action "updateCard" [board action]
  (apply-card-action board action
                     (fn [card]
                       (when (nil? card)
                         (println "updateCard action for unknown card!"))
                       ; we assume in other rewinders that
                       ; idMembers is always a set. (because
                       ; (conj nil :foo) gives (list :foo))
                       (merge (or card empty-card)
                              (-> action :data :old
                                  (update-in [:labels] set))))))

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
                     "removeChecklistFromCard"
                     "commentCard"
                     "copyCommentCard"
                     "makeAdminOfBoard"
                     "unconfirmedBoardInvitation"
                     "addMemberToBoard"
                     "addAttachmentToCard"
                     "deleteAttachmentFromCard"
                     "makeNormalMemberOfBoard"
                     "enablePowerUp"
                     "disablePowerUp"
                     "removeFromOrganizationBoard"
                     "addToOrganizationBoard"
                     "createBoard"})

(defmethod rewind-action :default [board action]
  (println (str "unknown action type " (action :type) (str " with data: " (pr-str action))))
  board)

;;;

(defn div [props & children]
  (apply dom/div (clj->js props) children))

(defn member-component [member owner]
  (om/component
   (if member
     (div {:className "member"}
        (if (member :avatarHash)
          (let [img-src (str "https://trello-avatars.s3.amazonaws.com/" (member :avatarHash) "/30.png")]
            (dom/img (clj->js {:src img-src})))
          (member :initials)))
     (div nil "?"))))

(defn cover-image-component [attachment owner]
  (om/component
   (let [preview (->> attachment
                      :previews
                      (filter #(-> % :width (>= 250)))
                      (apply min-key :width))
         image (or preview attachment)]

     (div {:className "cover-image"
           :style {:background-image (str "url(" (image :url) ")")
                   :background-color (attachment :edgeColor)
                   :height (or (image :height) 100)
                   }}))))

(defn card-component [card owner]
  (om/component
   (apply div {:className "card"}
          (keep identity
                [(let [labels (filter (card :labels) ["green" "yellow" "orange" "red" "purple" "blue"])]
                   (if (seq labels)
                     (apply div {:className "labels"}
                            (map #(div {:className (str "label-" %)}) labels))))
                 (if-let [cover-id (card :idAttachmentCover)]
                   (if-let [cover (-> card :attachments (get cover-id))]
                     (om/build cover-image-component cover)))
                 (div nil (card :name))
                 (let [members (card :members)]
                   (if (seq members)
                     (apply div {:className "member-container"}
                            (om/build-all member-component members))))
                 ]))))

(defn list-component [list owner]
  (om/component
   (div {:className "list"}
        (div nil
             (div {:className "list-title"}
                  (list :name))
             (apply div {:style {:overflow-y "auto"
                                 :flex "0 1 auto"}}
                    (om/build-all card-component (list :cards)))))))

(defn board-component [[board member-map] owner]
  (om/component
   (div {:className "board"}
        (div {:className "board-title"}
             (board :name))
        (apply div {:className "lists-container"}
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

(defn pretty-print-date [date-string]
  (let [js-date (js/Date. date-string)
        month-names ["Jan" "Feb" "Mar" "Apr" "May" "June" "July" "Aug" "Sep" "Oct" "Nov" "Dec"]
        month (month-names (.getMonth js-date))
        day (.getDate js-date)
        year (.getFullYear js-date)]
    (str month " " day " " year)))

(defn slider-component [{:keys [actions change-handler time-index]} _]
  (om/component
   (div nil (div {:className "timestamp"}
                 (if (= time-index (count actions))
                   "the beginning of time"
                   (pretty-print-date ((actions time-index) :date))))
        (dom/input #js {:type "range"
                        :min 0
                        :max (count actions)
                        :value (- (count actions) time-index)
                        :onChange (fn [e]
                                    (let [value (js/parseInt (.. e -target -value) 10)
                                          max-value (js/parseInt (.. e -target -max) 10)]
                                      (change-handler (- max-value value))))
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
                             board (reduce rewind-action (app-state :board) actions)]
                         (om/build board-component [board (app-state :members)]))))))

(om/root app-component
         app-state
         {:target (js/document.getElementById "my-app")})

(def board-id "dbdNNMrI")

(defn useful-actions [all-actions]
  (remove #(contains? no-op-actions (% :type)) all-actions))

(defn load-actions-before [date-string limit cb]
  (trello (str "boards/" board-id "/actions")
          {:before date-string
           :limit limit}
          cb))

(defn add-actions! [new-actions]
  (let [concat-actions #(vec (concat % (useful-actions new-actions)))]
    (swap! app-state #(update-in % [:actions] concat-actions))))

(defn load-all-actions! [date-string]
  (let [limit 500]
    (load-actions-before date-string
                         limit
                         (fn [new-actions]
                           (add-actions! new-actions)
                           (if (= (count new-actions) limit)
                             (load-all-actions! (-> new-actions last :date))
                             (println "done"))))))

(trello (str "boards/" board-id)
        {:lists "all"
         :list_fields "name,pos,closed"
         :cards "all"
         :card_fields "name,idList,pos,idMembers,closed,idAttachmentCover,labels"
         :card_attachments true
         :card_attachment_fields "url,previews,edgeColor"
         :members "all"
         :member_fields "avatarHash,initials"
         :actions "all"
         :actions_limit 10}
        (fn [board]
          (let [all-actions (board :actions)]
            (reset! app-state {:actions (vec (useful-actions all-actions))
                               :members (-> board :members vec->id-map)
                               :board (sanitize-board board)})
            (load-all-actions! (-> all-actions last :date)))))

