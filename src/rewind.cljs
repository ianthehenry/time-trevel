(ns main.rewind
  (:require [main.utils :refer [dissoc-in no-op-actions]]))

(defmulti rewind-action (fn [board {:keys [type]}] type))

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



(defmethod rewind-action :default [board action]
  (println (str "unknown action type " (action :type) (str " with data: " (pr-str action))))
  board)
