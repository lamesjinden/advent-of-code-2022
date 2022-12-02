(ns aoc.day02
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def day02-input-path "resources/day02.txt")

(def sample-input "A Y\nB X\nC Z")

"
A for Rock, B for Paper, and C for Scissors.
"
(def opponent-mapping {"A" :rock
                       "B" :paper
                       "C" :scissors})

"
X for Rock, Y for Paper, and Z for Scissors.
"
(def you-mapping {"X" :rock
                  "Y" :paper
                  "Z" :scissors})

(defn parse-line-part1 [line]
  (let [[first second] (str/split line #" ")]
    {:opponent (get opponent-mapping first)
     :you      (get you-mapping second)}))

"
(1 for Rock, 2 for Paper, and 3 for Scissors)
"
(def choice->score {:rock     1
                    :paper    2
                    :scissors 3})

"
(0 if you lost, 3 if the round was a draw, and 6 if you won).
"
(def outcome->score {:win  6
                     :draw 3
                     :lose 0})

(def win-states #{{:opponent :rock
                   :you      :paper}
                  {:opponent :paper
                   :you      :scissors}
                  {:opponent :scissors
                   :you      :rock}})

(defn evaluate-round-part1 [{:keys [opponent you] :as round}]
  (let [score-for-choice (get choice->score you)
        outcome (cond
                  (= opponent you) :draw
                  (contains? win-states round) :win
                  :else :lose)
        score-for-outcome (get outcome->score outcome)]
    (+ score-for-choice score-for-outcome)))

"
X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.
"
(def outcome-mapping {"X" :lose
                      "Y" :draw
                      "Z" :win})

(defn parse-line-part2 [line]
  (let [[first second] (str/split line #" ")]
    {:opponent (get opponent-mapping first)
     :outcome  (get outcome-mapping second)}))

(def outcome->choice {:win  {:rock     :paper
                             :paper    :scissors
                             :scissors :rock}
                      :lose {:rock     :scissors
                             :paper    :rock
                             :scissors :paper}
                      :draw {:rock     :rock
                             :paper    :paper
                             :scissors :scissors}})

(defn evaluate-round-part2 [{:keys [opponent outcome] :as _round}]
  (let [choice (get-in outcome->choice [outcome opponent])
        score-for-choice (get choice->score choice)
        score-for-outcome (get outcome->score outcome)]
    (+ score-for-choice score-for-outcome)))

(comment

  ;13565
  (let [score (->> (slurp day02-input-path)
                   (str/split-lines)
                   (map parse-line-part1)
                   (map evaluate-round-part1)
                   (reduce +))]
    score)

  ;12424
  (let [score (->> (slurp day02-input-path)
                   (str/split-lines)
                   (map parse-line-part2)
                   (map evaluate-round-part2)
                   (reduce +))]
    score)
  )

