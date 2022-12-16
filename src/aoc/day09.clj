(ns aoc.day09
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]
            [aoc.util :refer [->int] :as util]))

(def input-path "resources/day09.txt")

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def sample-input-2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(def default-starting-coordinates [0 0])

(defn parse-instruction [line]
  (let [[direction-token distance-token] (str/split line #" ")
        distance (->int distance-token)]
    (case direction-token
      "U" {:direction :up :distance distance}
      "D" {:direction :down :distance distance}
      "L" {:direction :left :distance distance}
      "R" {:direction :right :distance distance}
      (throw (ex-info (format "unknown direction: %s" direction-token) {})))))

(defn parse-instructions [s]
  (as-> s $
        (str/split-lines $)
        (map parse-instruction $)))

(def head-transforms
  "
  maps direction to 1 step coordinate transform description
  "
  {:left  [0 -1]
   :right [0 1]
   :up    [1 0]
   :down  [-1 0]})

(def tail-transforms
  "
  maps location differential to coordinate transform description;
  i.e. the difference between head coordinates and tail coordinates maps to
  the coordinate transform required to place tail
  "

  {
   ;; overlap
   [0 0]   [0 0]

   ;; adjacent
   [0 -1]  [0 0]                                            ; left
   [0 1]   [0 0]                                            ; right
   [1 0]   [0 0]                                            ; up
   [-1 0]  [0 0]                                            ; down
   [1 -1]  [0 0]                                            ; up and left
   [1 1]   [0 0]                                            ; up and right
   [-1 1]  [0 0]                                            ; down and right
   [-1 -1] [0 0]                                            ; down and left

   ;; inline, away 2
   [0 -2]  [0 -1]                                           ;; head is left 2 -> move left 1
   [0 2]   [0 1]                                            ;; head is right 2 -> move right 1
   [2 0]   [1 0]                                            ;; head is up 2 -> move up 1
   [-2 0]  [-1 0]                                           ;; head is down 2 -> move down 1

   ;; diagonal away (knight's move)
   [1 -2]  [1 -1]                                           ;; head is up 1, left 2 -> move up 1, left 1
   [-1 -2] [-1 -1]                                          ;; head is down 1, left 2  -> move down 1, left 1
   [1 2]   [1 1]                                            ;; head is up 1, right 2  -> move up 1, right 1
   [-1 2]  [-1 1]                                           ;; head is down 1, right 2  -> move down 1, right 1,
   [2 -1]  [1 -1]                                           ;; head is up 2, left 1 -> move up 1, left 1
   [2 1]   [1 1]                                            ;; head is up 2, right 1 -> move up 1, right 1
   [-2 -1] [-1 -1]                                          ;; head is down 2, left 1 -> move down 1, left 1
   [-2 1]  [-1 1]                                           ;; head is down 2, right 1 -> move down 1, right 1

   ;; p2 - diagonal away by 2
   [-2 -2] [-1 -1]                                          ;; leader is down 2, left 2 -> move down 1, left 1
   [-2 2]  [-1 1]                                           ;; leader is down 2, right 2 -> move down 1, right 1
   [2 2]   [1 1]                                            ;; leader is up 2, right 2 -> move up 1, right 1
   [2 -2]  [1 -1]                                           ;; leader is up 2, left 2 -> move up 1, left 1
   })

(defn coord-differential
  "
  returns the location differential between coords1 and coords2.
  coordinate differential is a 2 element vector describing the
  row-delta between coords1 and coords2, then the column delta
  between coords1 adn coords2.
  "
  [coords1 coords2]
  (m/sub coords1 coords2))

(defn apply-move
  "
  applies the transform description to coords;
  i.e. performs element-wise addition of 'coords' and 'transform.
  "
  [coords transform]
  (m/add coords transform))

;;; Part 1

(defn- simulate-intermediate-step-p1 [direction {:keys [head-coords tail-coords tail-locations] :as _state}]
  (let [head-next-transform (get head-transforms direction)
        head-next (apply-move head-coords head-next-transform)
        differential (coord-differential head-next tail-coords)
        tail-next-transform (get tail-transforms differential)
        tail-next (apply-move tail-coords tail-next-transform)]
    {:head-coords    head-next
     :tail-coords    tail-next
     :tail-locations (conj tail-locations tail-next)}))

(defn simulate-step-p1 [state {:keys [direction distance] :as _instruction}]
  ;; this is also a reduce... over (range 0 distance)
  (let [end-state (->> (range 0 distance)
                       (reduce (fn [state _i]
                                 (simulate-intermediate-step-p1 direction state))
                               state))]
    end-state))

(defn simulate-motions-p1 [starting-coordinates instructions]
  (let [head-coords starting-coordinates
        tail-coords starting-coordinates
        tail-locations #{}
        state {:head-coords    head-coords
               :tail-coords    tail-coords
               :tail-locations tail-locations}]
    (reduce simulate-step-p1 state instructions)))

;;; Part 2

(def default-knot-count 10)

(defn- simulate-intermediate-step-p2 [direction state]
  ;; bootstrap movement of the head position
  (let [[head-id {head-coords :coords}] (first state)
        head-next-transform (get head-transforms direction)
        head-next (apply-move head-coords head-next-transform)
        bootstrapped (assoc-in state [head-id :coords] head-next)
        head-tails (->> (keys state)
                        (partition 2 1))]

    ;; strictly update follower state in each [leader follower] pair
    (reduce (fn [state [leader-id follower-id]]
              (let [leader-coords (get-in state [leader-id :coords])
                    follower-coords (get-in state [follower-id :coords])
                    differential (coord-differential leader-coords follower-coords)
                    follower-next-transform (get tail-transforms differential)
                    follower-next (apply-move follower-coords follower-next-transform)]
                (-> state
                    (assoc-in [follower-id :coords] follower-next)
                    (update-in [follower-id :locations] conj follower-next))))
            bootstrapped
            head-tails)))

(defn simulate-step-p2 [state {:keys [direction distance] :as _instruction}]
  (->> (range 0 distance)
       (reduce (fn [state _i]
                 (simulate-intermediate-step-p2 direction state))
               state)))

(defn simulate-motions-p2 [starting-coordinates knot-count instructions]
  (let [state (->> (range 0 knot-count)
                   (map (fn [i]
                          {i {:coords    starting-coordinates
                              :locations #{}}}))
                   (into (sorted-map)))]
    (reduce simulate-step-p2 state instructions)))

(comment

  ;; After simulating the rope, you can count up all of the positions the tail visited at least once.

  ; 6067
  (let [instructions (parse-instructions (slurp input-path))
        simulated-state (simulate-motions-p1 default-starting-coordinates instructions)]
    (count (get-in simulated-state [:tail-locations])))

  ;; How many positions does the tail of the rope visit at least once?

  ;2471
  (let [instructions (parse-instructions (slurp input-path))
        simulated-state (simulate-motions-p2 default-starting-coordinates default-knot-count instructions)]
    (as-> (keys simulated-state) $
          (last $)
          (get-in simulated-state [$ :locations])
          (count $)))

  )