(ns main.core
  (:require
   [clojure.string :as string]
   [main.rewind :refer [rewind-action]]
   [main.trello :as trello]
   [main.utils :refer [no-op-actions vec->id-map sanitize-board]]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:board {} :actions []}))

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
  (trello/GET (str "boards/" board-id "/actions")
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

(trello/GET (str "boards/" board-id)
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

