(ns main.rewind
  (:require [main.utils :refer [dissoc-in
                                no-op-actions
                                sanitize-card
                                sanitize-list]]))

(defmulti rewind-action (fn [board action] (action :type)))

(defn extract-id [model]
  ; if model is nil, it returns nil.
  ; yeah, sometimes it's nil. ugh.
  (let [presumed-id (or (:id model)
                        (:_id model))]
    ; true story. see moveListToBoard actions from early 2013
    (if (map? presumed-id)
      (extract-id presumed-id)
      presumed-id)))

(defn card-id [action] (-> action :data :card extract-id))

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
                       (when (nil? card)
                         (println "addMemberToCard action for unknown card!"))
                       (update-in (or card empty-card) [:idMembers] #(disj % (-> action :data :idMember))))))

(defmethod rewind-action "removeMemberFromCard" [board action]
  (apply-card-action board action
                     (fn [card]
                       (when (nil? card)
                         (println "removeMemberFromCard action for unknown card!"))
                       (update-in (or card empty-card) [:idMembers] #(conj % (-> action :data :idMember))))))

(defmethod rewind-action "updateBoard" [board action]
  (merge board (-> action :data :old)))

(defmethod rewind-action "updateList" [board action]
  (let [list-id (-> action :data :list extract-id)]
    (if (nil? (-> board :lists (get list-id)))
      (do
        (println "updateList action for unknown list!")
        board)
      (update-in board [:lists list-id]
                 #(merge % (-> action :data :old))))))

(doseq [type ["createCard"
              "convertToCardFromCheckItem"
              "copyCard"
              "moveCardToBoard"]]
  (defmethod rewind-action type [board action]
    (dissoc-in board [:cards (card-id action)])))

(defn assoc-card [board action card-fn]
  (assoc-in board [:cards (card-id action)]
            (-> action :data :card sanitize-card
                (update-in [:idList] #(or % (-> action :data :list extract-id)))
                card-fn)))

(defmethod rewind-action "moveCardFromBoard" [board action]
  (assoc-card board action identity))

(defmethod rewind-action "deleteCard" [board action]
  (assoc-card board action #(assoc % :name "(deleted card)")))

(doseq [type ["moveListFromBoard"]]
  (defmethod rewind-action type [board action]
    (assoc-in board [:lists (-> action :data :list extract-id)] (-> action :data :list sanitize-list))))

(doseq [type ["createList"
              "moveListToBoard"]]
  (defmethod rewind-action type [board action]
    (dissoc-in board [:lists (-> action :data :list extract-id)])))

(defmethod rewind-action :default [board action]
  (println (str "unknown action type " (action :type) (str " with data: " (pr-str action))))
  board)
