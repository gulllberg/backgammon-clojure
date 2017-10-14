(ns backgammon.view
  (:require [backgammon.state :as state]))

(defn get-dice-url [eyes]
  (condp = eyes
    1 "https://upload.wikimedia.org/wikipedia/commons/1/1b/Dice-1-b.svg"
    2 "https://upload.wikimedia.org/wikipedia/commons/4/46/Dice-2a-b.svg"
    3 "https://upload.wikimedia.org/wikipedia/commons/c/c4/Dice-3a-b.svg"
    4 "https://upload.wikimedia.org/wikipedia/commons/f/fd/Dice-4-b.svg"
    5 "https://upload.wikimedia.org/wikipedia/commons/0/08/Dice-5-b.svg"
    6 "https://upload.wikimedia.org/wikipedia/commons/5/5a/Dice-6a-b.svg"))

(defn get-selected-dice-url [eyes]
  (condp = eyes
    1 "https://upload.wikimedia.org/wikipedia/commons/0/09/Dice-1.svg"
    2 "https://upload.wikimedia.org/wikipedia/commons/b/bc/Dice-2a.svg"
    3 "https://upload.wikimedia.org/wikipedia/commons/2/28/Dice-3a.svg"
    4 "https://upload.wikimedia.org/wikipedia/commons/1/16/Dice-4.svg"
    5 "https://upload.wikimedia.org/wikipedia/commons/d/dc/Dice-5.svg"
    6 "https://upload.wikimedia.org/wikipedia/commons/d/d7/Dice-6a.svg"))

(defn get-piece-url [owner]
  (condp = owner
    :white "assets/white.svg"
    :black "assets/black.svg"))

(defn get-selected-piece-url [owner]
  (condp = owner
    :white "assets/white-selected.svg"
    :black "assets/black-selected.svg"))

(defn get-board-url []
  "assets/board.png")

(defn get-captured-piece-position [owner]
  {:left "45%"
   :top  (condp = owner
           :white "51%"
           :black "33%")})

(defn get-piece-position [square-index]
  {:left (condp = square-index
           0 "88.5%"
           1 "81%"
           2 "73.5%"
           3 "66%"
           4 "58.5%"
           5 "51%"
           6 "39%"
           7 "31.5%"
           8 "24%"
           9 "16.5%"
           10 "9%"
           11 "1.5%"
           12 "1.5%"
           13 "9%"
           14 "16.5%"
           15 "24%"
           16 "31.5%"
           17 "39%"
           18 "51%"
           19 "58.5%"
           20 "66%"
           21 "73.5%"
           22 "81%"
           23 "88.5%")
   :top  (if (>= square-index 12)
           "3%"
           "82.5%")})

(defn captured-piece [{view-state :view-state owner :owner trigger-event :trigger-event}]
  (let [game-state (:game-state view-state)
        captured-piece-height (state/get-captured-pieces-of-player game-state owner)]
    (when (not= 0 captured-piece-height)
      [:div {:style (merge {:position "absolute"
                            :height   "100px"
                            :width    "100px"}
                           (get-captured-piece-position owner))}
       [:img {:src    (if (state/captured-piece-index-of-player? game-state owner (:selected-piece-index view-state))
                        (get-selected-piece-url owner)
                        (get-piece-url owner))
              :width  "100px"
              :height "100px"
              :style  {:position "absolute"}}]
       [:div {:style   {:font-size "50px"
                        :position  "absolute"
                        :top       "20%"
                        :left      "38%"
                        :cursor    "pointer"}
              :onClick (fn []
                         (trigger-event {:event :piece-click :data (state/get-captured-piece-index-of-player game-state owner)}))}
        captured-piece-height]])))

(defn piece [{view-state :view-state square-index :square-index trigger-event :trigger-event}]
  (let [game-state (:game-state view-state)
        square-height (state/get-square-height game-state square-index)]
    (when (not= 0 square-height)
      [:div {:style (merge {:position "absolute"
                            :height   "100px"
                            :width    "100px"}
                           (get-piece-position square-index))}
       [:img {:src    (if (= (:selected-piece-index view-state) square-index)
                        (get-selected-piece-url (state/get-square-owner game-state square-index))
                        (get-piece-url (state/get-square-owner game-state square-index)))
              :width  "100px"
              :height "100px"
              :style  {:position "absolute"}}]
       [:div {:style   {:font-size "50px"
                        :position  "absolute"
                        :top       "20%"
                        :left      "38%"
                        :cursor    "pointer"}
              :onClick (fn []
                         (trigger-event {:event :piece-click :data square-index}))}
        square-height]]
      )))

(defn dice [{view-state :view-state trigger-event :trigger-event}]
  (let [game-state (:game-state view-state)]
    [:div
     [:span {:style {:background-color (condp = (state/get-player-in-turn game-state)
                                         :white "ghostwhite"
                                         :black "salmon")
                     :font-size        "100px"}}
      "Player in turn"]
     (map-indexed (fn [index move]
                    [:span {:style   {:height      "100px"
                                      :width       "100px"
                                      :cursor      "pointer"
                                      :margin-left "10px"}
                            :onClick (fn []
                                       (trigger-event {:event :dice-click :data index}))
                            :key     index}
                     [:img {:src    (if (= (:selected-dice-index view-state) index)
                                      (get-selected-dice-url move)
                                      (get-dice-url move))
                            :height "100px"
                            :width  "100px"}]])
                  (state/get-remaining-moves game-state))]))

(defn board [{state-atom    :state-atom
              trigger-event :trigger-event}]
  (let [view-state (deref state-atom)]
    [:div
     [dice {:view-state view-state :trigger-event trigger-event}]
     [:div {:style {:position "relative"
                    :width    "1000px"
                    :height   "687px"}}
      [:img {:src    (get-board-url)
             :width  "1000px"
             :height "687px"
             :style  {:position "absolute"
                      :top      "0%"
                      :left     "0%"}}]
      (for [square-index (range 24)]
        ^{:key square-index} [piece {:view-state view-state :square-index square-index :trigger-event trigger-event}])
      [captured-piece {:view-state view-state :owner :white :trigger-event trigger-event}]
      [captured-piece {:view-state view-state :owner :black :trigger-event trigger-event}]]]))