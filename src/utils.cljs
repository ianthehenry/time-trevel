(ns main.utils)

; TODO: Figure out how to include the clojure core incubator as a dependency.
;       This is just copied and pasted :(
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
                     "updateChecklist"
                     "createBoard"})

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

(defn sanitize-list [list]
  (-> list
      (assoc :id (or (list :id)
                     (list :_id)))
      (dissoc :_id)))

(defn sanitize-board [board]
  (let [cards (->> board :cards (map sanitize-card) vec->id-map)
        lists (->> board :lists vec->id-map)]
    (-> board
        (merge {:cards cards
                :lists lists})
        (dissoc :actions :members))))
