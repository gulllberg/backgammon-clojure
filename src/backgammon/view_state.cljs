(ns backgammon.view-state
  (:require [backgammon.state :as state]
            [backgammon.core :as core]))

(defn create-view-state []
  {:game-state           (state/create-standard-state)
   :selected-piece-index nil
   :selected-dice-index  nil})

(defn should-move? [view-state]
  (and (not (nil? (:selected-piece-index view-state)))
       (not (nil? (:selected-dice-index view-state)))))

(defn move [view-state]
  (-> (update view-state :game-state core/move (:selected-piece-index view-state) (nth (state/get-remaining-moves (:game-state view-state)) (:selected-dice-index view-state)))
      (assoc :selected-piece-index nil :selected-dice-index nil)))

;; TODO should be in normal state
(defn get-owner [state square-index]
  (if (state/captured-piece-index? state square-index)
    (condp = square-index
      (state/get-captured-piece-index-of-player state :white) :white
      (state/get-captured-piece-index-of-player state :black) :black)
    (state/get-square-owner state square-index)))

(defn handle-piece-click [view-state square-index]
  (if (= (state/get-player-in-turn (:game-state view-state)) (get-owner (:game-state view-state) square-index))
    (assoc view-state :selected-piece-index square-index)
    view-state))

(defn handle-dice-click [view-state dice-index]
  (assoc view-state :selected-dice-index dice-index))