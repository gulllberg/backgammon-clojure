(ns backgammon.state
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [remove-one]]
            [clojure.string :refer [split]]))

(defn create-board
  {:test (fn []
           (is= (create-board "5w . . 3b 2w")
                [{:owner :white :height 5}
                 nil
                 nil
                 {:owner :black :height 3}
                 {:owner :white :height 2}])
           ;; TODO Implement for more than 9 pieces in a pile (maybe butlast?)
           ;; TODO Check if black actually is black (not just not-white)
           ;; TODO Get rid of horrible character handling from first/last of string
           ;; TODO Use (count) to get string length
           )}
  [board-string]
  (->> (split board-string #" ")
       (map (fn [square-string]
              (if (= square-string ".")
                nil
                {:owner (if (= (str (last square-string)) "w") :white :black)
                 :height (- (int (first square-string)) 48)})))
       (into [])))

(defn create-standard-board []
  ;; Tested by create-standard-state
  (create-board "2w . . . . 5b . 3b . . . 5w 5b . . . 3w . 5w . . . . 2b"))

(defn create-state
  ;; Tested by create-standard-state
  [{player-in-turn :player-in-turn
    board :board
    dice-roll :dice-roll
    remaining-moves :remaining-moves
    captured-pieces :captured-pieces
    home-board-length :home-board-length}]
  {:player-in-turn player-in-turn
   :board board
   :dice-roll dice-roll
   :remaining-moves remaining-moves
   :captured-pieces captured-pieces
   :home-board-length home-board-length})

(defn create-standard-state
  {:test (fn []
           (is= (create-standard-state)
                (create-state {:player-in-turn :white
                               :board (create-standard-board)
                               :dice-roll []
                               :remaining-moves []
                               :captured-pieces {:white 0
                                                 :black 0}
                               :home-board-length 6})))}
  []
  (create-state {:player-in-turn :white
                 :board (create-standard-board)
                 :dice-roll []
                 :remaining-moves []
                 :captured-pieces {:white 0
                                   :black 0}
                 :home-board-length 6}))

(defn get-square-data
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-square-data 0))
                {:owner :white :height 2})

           (is= (-> (create-standard-state)
                    (get-square-data 3))
                nil))}

  [state square-index]
  (nth (:board state) square-index))

(defn get-square-height
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-square-height 0))
                2)

           (is= (-> (create-standard-state)
                    (get-square-height 3))
                0))}

  [state square-index]
  (let [square-data (get-square-data state square-index)]
    (if (= square-data nil)
      0
      (:height square-data))))

(defn get-square-owner
  {:test (fn  []
           (is= (-> (create-standard-state)
                    (get-square-owner 0))
                :white)

           (is= (-> (create-standard-state)
                    (get-square-owner 1))
                nil))}

  [state square-index]
  (let [square-data (get-square-data state square-index)]
    (if (= square-data nil)
      nil
      (:owner square-data))))

(defn decrease-pile
  {:test (fn []
           (let [square-data
                 (-> (create-standard-state)
                     (decrease-pile 0)
                     (:board)
                     (nth 0))]
             (is= (:owner square-data) :white)
             (is= (:height square-data) 1))

           (let [square-data
                 (-> (create-standard-state)
                     (decrease-pile 0)
                     (decrease-pile 0)
                     (:board)
                     (nth 0))]
             (is= square-data nil)))}

  [state square-index]
  (let [square-data (get-square-data state square-index)
        new-square-data (if (or (= square-data nil)
                                (= (:height square-data) 1))
                          nil
                          (update square-data :height dec))]
    (assoc-in state [:board square-index] new-square-data)))

(defn increase-pile
  {:test (fn []
           (is= (-> (create-standard-state)
                    (increase-pile {:square-index 0})
                    (get-square-height 0))
                3)

           (is= (-> (create-standard-state)
                    (increase-pile {:square-index 3 :owner :white})
                    (get-square-data 3))
                {:owner :white
                 :height 1}))}

  [state {square-index :square-index owner :owner}]
  (let [square-data (get-square-data state square-index)
        new-square-data (if (= square-data nil)
                          {:height 1 :owner owner}
                          (update square-data :height inc))]
    (assoc-in state [:board square-index] new-square-data)))

(defn get-remaining-moves
  [state]
  (:remaining-moves state))

(defn get-dice-roll
  [state]
  (:dice-roll state))

(defn get-player-in-turn
  [state]
  (:player-in-turn state))

(defn set-dice-roll
  [state dice-roll]
  (assoc state :dice-roll dice-roll))

(defn set-player-in-turn
  [state player-in-turn]
  (assoc state :player-in-turn player-in-turn))

(defn set-remaining-moves
  [state remaining-moves]
  (assoc state :remaining-moves remaining-moves))

(defn remove-remaining-move
  {:test (fn []
           (is= (-> (create-standard-state)
                    (set-remaining-moves [1 2])
                    (remove-remaining-move 2))
                (-> (create-standard-state)
                    (set-remaining-moves [1])))

           (is= (-> (create-standard-state)
                    (set-remaining-moves [6 6 6 6])
                    (remove-remaining-move 6))
                (-> (create-standard-state)
                    (set-remaining-moves [6 6 6])))

           (is= (-> (create-standard-state)
                    (set-remaining-moves [1 2])
                    (remove-remaining-move 3))
                (-> (create-standard-state)
                    (set-remaining-moves [1 2]))))}

  [state move-to-remove]
  (let [new-remaining-moves
        (remove-one (:remaining-moves state) move-to-remove)]
    (assoc state :remaining-moves new-remaining-moves)))

(defn on-board?
  {:test (fn []
           (is (-> (create-standard-state)
                   (on-board? 0)))

           (is-not (-> (create-standard-state)
                       (on-board? 100)))

           (is-not (-> (create-standard-state)
                       (on-board? -1))))}
  [state square-index]
  (and (>= square-index 0)
       (< square-index (count (:board state)))))

(defn get-captured-pieces
  [state]
  (:captured-pieces state))

(defn get-captured-pieces-of-player
  {:test (fn []
           (is= (-> (create-state {:captured-pieces {:white 1
                                                     :black 0}})
                    (get-captured-pieces-of-player :white))
                1)

           (is= (-> (create-state {:captured-pieces {:white 1
                                                     :black 0}})
                    (get-captured-pieces-of-player :black))
                0))}

  [state player]
  (player (get-captured-pieces state)))

(defn decrease-captured-pieces-of-player
  {:test (fn []
           (is= (-> (create-state {:captured-pieces {:white 1}})
                    (decrease-captured-pieces-of-player :white)
                    (get-captured-pieces-of-player :white))
                0)

           ;; Can't decrease below 0
           (is= (-> (create-state {:captured-pieces {:white 0}})
                    (decrease-captured-pieces-of-player :white)
                    (get-captured-pieces-of-player :white))
                0))}

  [state player]
  (if (= (get-captured-pieces-of-player state player) 0)
    state
    (update-in state [:captured-pieces player] dec)))

(defn increase-captured-pieces-of-player
  {:test (fn []
           (is= (-> (create-state {:captured-pieces {:white 1}})
                    (increase-captured-pieces-of-player :white)
                    (get-captured-pieces-of-player :white))
                2))}

  [state player]
  (update-in state [:captured-pieces player] inc))

(defn get-opponent
  {:test (fn []
           (is= (get-opponent :white) :black)

           (is= (get-opponent :black) :white))}

  [player]
  (if (= player :white)
    :black
    :white))

;; TODO Kan ta bort funktion?
(defn captured-piece-index?
  {:test (fn []
           (is (-> (create-state {:board (create-board ".")})
                   (captured-piece-index? -1)))

           (is (-> (create-state {:board (create-board ".")})
                   (captured-piece-index? 1)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index? -2)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index? 0)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index? 2))))}

  [state square-index]
  (or (= (count (:board state)) square-index)
      (= square-index -1)))

(defn captured-piece-index-of-player?
  {:test (fn []
           (is (-> (create-state {:board (create-board ".")})
                   (captured-piece-index-of-player? :white -1)))

           (is (-> (create-state {:board (create-board ".")})
                   (captured-piece-index-of-player? :black 1)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index-of-player? :white 0)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index-of-player? :white 1)))

           (is-not (-> (create-state {:board (create-board ".")})
                       (captured-piece-index-of-player? :white -2))))}

  [state player square-index]
  (if (= player :white)
    (= square-index -1)
    (= (count (:board state)) square-index)))

(defn get-captured-piece-index-of-player
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-captured-piece-index-of-player :white))
                -1)
           (is= (-> (create-standard-state)
                    (get-captured-piece-index-of-player :black))
                24))}
  [state player]
  (if (= player :white)
    -1
    (count (:board state))))

(defn player-has-captured-pieces?
  {:test (fn []
           (is (-> (create-state {:captured-pieces {:white 1}})
                   (player-has-captured-pieces? :white)))

           (is-not (-> (create-state {:captured-pieces {:white 0}})
                       (player-has-captured-pieces? :white))))}

  [state player]
  (> (get-captured-pieces-of-player state player) 0))

(defn get-pieces-square-indices-of-player
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-pieces-square-indices-of-player :white))
                [0 11 16 18]))}

  [state player]
  (->> (:board state)
       (map-indexed (fn [index square-data]
                      (if (= (:owner square-data) player)
                        index
                        nil)))
       (remove nil?)))

(defn get-home-board-length
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-home-board-length))
                6))}
  [state]
  (:home-board-length state))

(defn set-home-board-length
  {:test (fn []
           (is= (-> (create-standard-state)
                    (set-home-board-length 10)
                    (get-home-board-length))
                10))}
  [state home-board-length]
  (assoc state :home-board-length home-board-length))

(defn get-board-length
  {:test (fn []
           (is= (-> (create-standard-state)
                    (get-board-length))
                24))}
  [state]
  (count (:board state)))

;; TODO Make test better
(defn player-has-pieces-outside-home-board?
  {:test (fn []
           (is (-> (create-standard-state)
                   (player-has-pieces-outside-home-board? :white)))
           (is (-> (create-standard-state)
                   (player-has-pieces-outside-home-board? :black)))
           (is-not (-> (create-standard-state)
                       (set-home-board-length 24)
                       (player-has-pieces-outside-home-board? :white)))
           (is-not (-> (create-standard-state)
                       (set-home-board-length 24)
                       (player-has-pieces-outside-home-board? :black))))}
  [state player]
  (let [pieces-square-indices (get-pieces-square-indices-of-player state player)
        home-board-length (get-home-board-length state)]
    (if (= player :white)
      (some (fn [index]
              (< index (- (get-board-length state) home-board-length)))
            pieces-square-indices)
      (some (fn [index]
              (>= index home-board-length))
            pieces-square-indices))))

(defn player-has-pieces-remaining?
  {:test (fn []
           (is (-> (create-standard-state)
                   (player-has-pieces-remaining? :white)))
           (is (-> (create-state {:board (create-board ". 1b")
                                  :captured-pieces {:white 1
                                                    :black 0}})
                   (player-has-pieces-remaining? :white)))
           (is-not (-> (create-state {:board (create-board ". 1b")
                                      :captured-pieces {:white 0
                                                        :black 0}})
                       (player-has-pieces-remaining? :white))))}
  [state player]
  (or (player-has-captured-pieces? state player)
      (> (count (get-pieces-square-indices-of-player state player)) 0)))
