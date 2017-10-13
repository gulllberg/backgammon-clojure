(ns backgammon.play
  (:require [backgammon.core :as c]
            [backgammon.state :as s]
            [clojure.core.async :refer [go <! timeout]]))

(def state-atom (atom (c/create-state-for-first-turn)))

(defn draw-board
  ;; TODO Should not only work for board on standard format
  [state]
  (let [range->output (fn [range]
                        (reduce (fn [a index]
                                  (str a
                                       (let [square-data (s/get-square-data state index)
                                             owner (:owner square-data)]
                                         (cond
                                           (= owner :white)
                                           (str (:height square-data) "w ")

                                           (= owner :black)
                                           (str (:height square-data) "b ")

                                           :default
                                           ". "))))
                                ""
                                range))]

    (println (str "Player in turn: " (s/get-player-in-turn state)))
    (println (str "Remaining moves: ") (s/get-remaining-moves state))
    (println "--------------------------------")
    (println (str "Captured pieces of black: " (s/get-captured-pieces-of-player state :black)))
    (println "--------------------------------")
    (println (str (range->output (range 12 18)) "| " (range->output (range 18 24))))
    (println "--------------------------------")
    (println (str (range->output (range 11 5 -1)) "| " (range->output (range 5 -1 -1))))
    (println "--------------------------------")
    (println (str "Captured pieces of white: " (s/get-captured-pieces-of-player state :white)))
    (println "--------------------------------")))

(defn play-move
  [from-square distance]
  (let [from-index (- from-square 1)]
    (swap! state-atom c/move from-index distance)
    (if (c/player-has-won? (deref state-atom) (s/get-player-in-turn (deref state-atom)))
      ((fn []
         (println "********************************")
         (println (str (s/get-player-in-turn (deref state-atom)) " has won!!! :D"))
         (println "********************************")))
      (when (c/end-of-turn? (deref state-atom))
        (swap! state-atom c/end-turn-and-start-new-turn))))
  (draw-board (deref state-atom)))

(add-watch state-atom :game-engine
           (fn [_ _ _ state]
             (draw-board state)
             (cond
               (c/player-has-won? state (s/get-player-in-turn state))
               ((fn []
                  (println "********************************")
                  (println (str (s/get-player-in-turn (deref state-atom)) " has won!!! :D"))
                  (println "********************************")))

               (c/end-of-turn? state)
               (swap! state-atom c/end-turn-and-start-new-turn)

               :default
               (let [movable-pieces-square-indices (c/get-movable-pieces-square-indices-of-player state (s/get-player-in-turn state))
                     from-index (first (sort movable-pieces-square-indices))
                     distance (last (sort (c/get-all-valid-moves-from-square state from-index)))]
                 (go (<! (timeout 1000))
                     (swap! state-atom c/move from-index distance))))))

(comment

  (swap! state-atom (fn [state]
                      (assoc state :board (s/create-board ". . . . . . . 3b . . . . . . . . . . . . . . . 2w"))))

  (swap! state-atom (fn [state]
                      (assoc state :board (s/create-standard-board))))

  (draw-board  (deref state-atom))
  (play-move 0 3)

  "")
