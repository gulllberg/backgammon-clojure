(ns backgammon.core
  (:require [backgammon.state :as s]
            [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(defn toggle-player-in-turn
  {:test (fn []
           (is= (-> (s/create-standard-state)
                    (toggle-player-in-turn))
                (s/create-state {:player-in-turn :black
                                 :board (s/create-standard-board)
                                 :remaining-moves []
                                 :dice-roll []
                                 :captured-pieces {:white 0
                                                   :black 0}
                                 :home-board-length 6})))}

  [state]
  (let [player-in-turn (s/get-player-in-turn state)]
    (if (= player-in-turn :white)
      (s/set-player-in-turn state :black)
      (s/set-player-in-turn state :white))))

;; TODO
;; If moves can be made according to either one die or the other, but not both,
;; the higher number must be used. If one die is unable to be moved, but such a
;; move is made possible by the moving of the other die, that move is compulsory.
(defn valid-move?
  {:test (fn []
           ;; With a captured piece you must move it first
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "5w .")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 1
                                                          :black 0}})
                       (valid-move? 0 1)))

           ;; Cannot move to square occupied by two enemy pieces
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "4w 2b")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 0
                                                          :black 0}})
                       (valid-move? 0 1)))

           ;; Wrong player-in-turn
           (is-not (-> (s/create-state {:player-in-turn :black
                                        :board (s/create-board "5w .")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 0
                                                          :black 0}})
                       (valid-move? 0 1)))

           ;; From-index out of bounds
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "5w .")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 0
                                                          :black 0}})
                       (valid-move? 50 1)))

           ;; Attempt to move too far (outside board) with pieces outside home board
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "5w .")
                                        :remaining-moves [10]
                                        :captured-pieces {:white 0
                                                          :black 0}
                                        :home-board-length 0})
                       (valid-move? 0 10)))

           ;; Attempt to make a move not available in remaining moves
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "5w .")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 0
                                                          :black 0}})
                       (valid-move? 0 2)))

           ;; Cannot move a captured piece if you have none
           (is-not (-> (s/create-state {:player-in-turn :white
                                        :board (s/create-board "5w .")
                                        :remaining-moves [1]
                                        :captured-pieces {:white 0
                                                          :black 0}})
                       (valid-move? -1 1)))

           ;; Valid move
           (is (-> (s/create-state {:player-in-turn :white
                                    :board (s/create-board "5w .")
                                    :remaining-moves [1]
                                    :captured-pieces {:white 0
                                                      :black 0}})
                   (valid-move? 0 1)))

           ;; Can move piece outside board if all pieces inside home board
           (is (-> (s/create-state {:player-in-turn :white
                                    :board (s/create-board "5w .")
                                    :remaining-moves [2]
                                    :captured-pieces {:white 0
                                                      :black 0}
                                    :home-board-length 2})
                   (valid-move? 0 2))))}

  [state from-index distance]
  (let [player-in-turn (s/get-player-in-turn state)
        direction-operator (if (= player-in-turn :white) + -)
        to-index (direction-operator from-index distance)]
    (cond

      ;; If you have a captured piece you must move it
      (and (s/player-has-captured-pieces? state player-in-turn)
           (not (s/captured-piece-index-of-player? state player-in-turn from-index)))
      ;; ((fn [] (println "You must move a captured piece when you have one") false))
      false

      (and (s/captured-piece-index-of-player? state player-in-turn from-index)
           (not (s/player-has-captured-pieces? state player-in-turn)))
      ;; ((fn [] (println "You cannot move a captured piece if you have none") false))
      false

      (not (or (s/on-board? state from-index)
               (s/captured-piece-index? state from-index)))
      ;; ((fn [] (println "Not a valid index to move from") false))
      false

      (not (seq-contains? (s/get-remaining-moves state) distance))
      ;; ((fn [] (println "Distance not a remaining move") false))
      false

      (not (or (s/captured-piece-index-of-player? state player-in-turn from-index)
               (= (s/get-square-owner state from-index) player-in-turn)))
      ;; ((fn [] (println "Wrong player in turn, you must move your own pieces") false))
      false

      (not (s/on-board? state to-index))
      (if (s/player-has-pieces-outside-home-board? state player-in-turn)
        ;;  ((fn [] (println "You cannot move outside the board with pieces outside your home board") false))
        false
        true)

      (and (not= player-in-turn (s/get-square-owner state to-index))
           (>= (s/get-square-height state to-index) 2))
      ;; ((fn [] (println "You cannot move to a square where your opponent has two or more pieces") false))
      false

      :else
      true)))

(defn move
  {:test (fn []
           ;; White moves to empty square
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "5w .")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 0 1))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "4w 1w")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; White removes piece from board
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "5w .")
                                     :remaining-moves [2]
                                     :captured-pieces {:white 0
                                                       :black 0}
                                     :home-board-length 2})
                    (move 0 2))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "4w .")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}
                                 :home-board-length 2}))

           ;; Black moves to empty square
           (is= (-> (s/create-state {:player-in-turn :black
                                     :board (s/create-board ". 5b")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 1 1))
                (s/create-state {:player-in-turn :black
                                 :board (s/create-board "1b 4b")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; Move to square occupied by own piece
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "4w 1w")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 0 1))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "3w 2w")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; Can jump over two enemy pieces
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "4w 2b .")
                                     :remaining-moves [2]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 0 2))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "3w 2b 1w")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; Capture enemy piece
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "4w 1b")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 0 1))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "3w 1w")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 1}}))

           ;; White moves a captured piece
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "5w .")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 1
                                                       :black 0}})
                    (move -1 1))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "6w .")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; Black moves a captured piece
           (is= (-> (s/create-state {:player-in-turn :black
                                     :board (s/create-board ". .")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 1}})
                    (move 2 1))
                (s/create-state {:player-in-turn :black
                                 :board (s/create-board ". 1b")
                                 :remaining-moves []
                                 :captured-pieces {:white 0
                                                   :black 0}}))

           ;; Not a valid move, should return same state
           (is= (-> (s/create-state {:player-in-turn :white
                                     :board (s/create-board "5w .")
                                     :remaining-moves [1]
                                     :captured-pieces {:white 0
                                                       :black 0}})
                    (move 0 2))
                (s/create-state {:player-in-turn :white
                                 :board (s/create-board "5w .")
                                 :remaining-moves [1]
                                 :captured-pieces {:white 0
                                                   :black 0}})))}

  [state from-index distance]
  (let [player-in-turn (s/get-player-in-turn state)
        direction-operator (if (= player-in-turn :white) + -)
        to-index (direction-operator from-index distance)
        decrease-pile-or-captured-pieces (fn [state square-index]
                                           (if (s/on-board? state square-index)
                                             (s/decrease-pile state square-index)
                                             (s/decrease-captured-pieces-of-player state player-in-turn)))]

    (cond

      (not (valid-move? state from-index distance))
      ((fn [] (println "Not a valid move") state))

      ;; Remove pice from board
      (not (s/on-board? state to-index))
      (-> (decrease-pile-or-captured-pieces state from-index)
          (s/remove-remaining-move distance))

      ;; Should capture enemy piece
      (and (not= player-in-turn (s/get-square-owner state to-index))
           (= (s/get-square-height state to-index) 1))
      (-> (s/decrease-pile state to-index)
          (s/increase-captured-pieces-of-player (s/get-opponent player-in-turn))
          (decrease-pile-or-captured-pieces from-index)
          (s/increase-pile {:square-index to-index
                            :owner player-in-turn})
          (s/remove-remaining-move distance))

      :else
      (-> (decrease-pile-or-captured-pieces state from-index)
          (s/increase-pile {:square-index (direction-operator from-index distance)
                            :owner player-in-turn})
          (s/remove-remaining-move distance)))))

(defn roll-a-die
  ;; TODO Test av random funktion (kräver seed i state?)
  []
  (inc (rand-int 6)))

(defn determine-moves-from-dice-rolls
  {:test (fn []
           (is= (determine-moves-from-dice-rolls 1 2)
                [1 2])

           (is= (determine-moves-from-dice-rolls 1 1)
                [1 1 1 1]))}

  [first-roll second-roll]
  (if (= first-roll second-roll)
    [first-roll second-roll first-roll second-roll]
    [first-roll second-roll]))

;; TODO Ta bort funktion?
(defn roll-dice-and-determine-moves
  ;; TODO Test av random funktion
  []
  (determine-moves-from-dice-rolls (roll-a-die) (roll-a-die)))

(defn end-turn-and-start-new-turn
  ;; TODO Test av random funktion
  [state]
  (let [first-roll (roll-a-die)
        second-roll (roll-a-die)]
    (-> (toggle-player-in-turn state)
        (s/set-dice-roll [first-roll second-roll])
        (s/set-remaining-moves (determine-moves-from-dice-rolls first-roll second-roll)))))

(defn get-all-valid-moves-from-square
  {:test (fn []
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [2 3 5])
                    (get-all-valid-moves-from-square 0))
                [2 3])

           ;; No valid moves from an empty square
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [2 3 5])
                    (get-all-valid-moves-from-square 1))
                [])

           ;; No valid moves if you are not in turn
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [2 3 5])
                    (get-all-valid-moves-from-square 1))
                []))}

  [state square-index]
  (filter (fn [distance]
            (valid-move? state square-index distance))
          (s/get-remaining-moves state)))

(defn get-movable-pieces-square-indices-of-player
  {:test (fn []
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [5])
                    (get-movable-pieces-square-indices-of-player :white))
                [11 16])
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [100])
                    (get-movable-pieces-square-indices-of-player :white))
                [])
           (is= (-> (s/create-standard-state)
                    (s/set-remaining-moves [5])
                    (s/increase-captured-pieces-of-player :white)
                    (get-movable-pieces-square-indices-of-player :white))
                [-1]))}

  [state player]
  (let [remaining-moves (s/get-remaining-moves state)
        square-index-has-valid-move? (fn [square-index]
                                       (reduce (fn [a v]
                                                 (or a (valid-move? state square-index v)))
                                               false
                                               remaining-moves))]
    (if (s/player-has-captured-pieces? state player)
      (filter square-index-has-valid-move? [(s/get-captured-piece-index-of-player state player)])
      (->> (s/get-pieces-square-indices-of-player state player)
           (filter square-index-has-valid-move?)))))

(defn end-of-turn?
  {:test (fn []
           (is (-> (s/create-standard-state)
                   (end-of-turn?)))

           (is-not (-> (s/create-standard-state)
                       (s/set-remaining-moves [1])
                       (end-of-turn?)))

           (is (-> (s/create-standard-state)
                   (s/set-remaining-moves [100])
                   (end-of-turn?))))}

  [state]
  (= (count (get-movable-pieces-square-indices-of-player state (s/get-player-in-turn state))) 0))

(defn create-state-for-first-turn
  []
  (-> (s/create-standard-state)
      (s/set-remaining-moves (roll-dice-and-determine-moves))))

(defn player-has-won?
  {:test (fn []
           (is-not (-> (s/create-standard-state)
                       (player-has-won? :white)))
           (is (-> (s/create-state {:board (s/create-board ". 1b")
                                    :captured-pieces {:white 0
                                                      :black 0}})
                   (player-has-won? :white))))}
  [state player]
  (not (s/player-has-pieces-remaining? state player)))
