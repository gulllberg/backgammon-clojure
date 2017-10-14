(ns backgammon.main
  (:require [reagent.core :as reagent :refer [atom]]
            [backgammon.view :as view]
            [backgammon.state :as state]
            [backgammon.core :as core]
            [backgammon.view-state :as view-state]))

(enable-console-print!)

(defonce app-state-atom (atom nil))

(add-watch app-state-atom :game-engine
           (fn [_ _ _ view-state]
             (cond
               (core/player-has-won? (:game-state view-state) (state/get-player-in-turn (:game-state view-state)))
               (println (str "Player " (state/get-player-in-turn (:game-state view-state)) " has won!! :D"))

               (core/end-of-turn? (:game-state view-state))
               (swap! app-state-atom (fn [app-state]
                                       (update app-state :game-state core/end-turn-and-start-new-turn)))

               (view-state/should-move? view-state)
               (swap! app-state-atom view-state/move)
               )))

(swap! app-state-atom
       (fn [state]
         (or state (view-state/create-view-state))))

(defn handle-event!
  [{event :event data :data}]
  (condp = event
    :piece-click
    (swap! app-state-atom view-state/handle-piece-click data)

    :dice-click
    (swap! app-state-atom view-state/handle-dice-click data)
    ))

(reagent/render-component [view/board {:state-atom    app-state-atom
                                       :trigger-event (fn [{event :event data :data :as params}]
                                                        (println event data)
                                                        (handle-event! params))}]
                          (. js/document (getElementById "app")))
