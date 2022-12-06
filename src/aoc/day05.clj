(ns aoc.day05
  (:require [clojure.string :as str]
            [aoc.util :refer [->int drop-nil] :as util]))

(def input-path "resources/day05.txt")

(def sample-input "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
;                                            0123456789
;                                             ^   ^   ^
; stack 1: 1 = 1 + 4*0 where 0 is (stack-id|1 - 1)
; stack 2: 5 = 1 + 4*1 where 0 is (stack-id|2 - 1)
; stack 3: 9 = 1 + 4*2 where 0 is (stack-id|3 - 1)

(defn parse-stacks
  "
  returns a map from stack-id:int to seq of crate-id:string.
  values are ordered with the bottom-most element at the head of the seq.

  for example, when parsed, the sample input yields:
  {1 (Z N),
   2 (M C D),
   3 (P)}
  "
  [lines]
  (let [starting-state-input lines
        stacks-state-input-lines (->> starting-state-input
                                      (drop-last)
                                      (mapv vec))
        stack-ids (as-> starting-state-input $
                        (last $)
                        (util/split $ #" " :remove-empty true)
                        (map ->int $))
        stacks (->> stack-ids
                    (map (fn [id]
                           (let [x-index (+ 1 (* 4 (dec id)))
                                 crate-ids (->> (for [y-index (range 0 (count stacks-state-input-lines))]
                                                  (let [cell (get-in stacks-state-input-lines [y-index x-index] \space)]
                                                    (when (Character/isUpperCase ^Character cell)
                                                      (str cell))))
                                                (drop-nil)
                                                (reverse))]
                             [id crate-ids])))
                    ;; note
                    ;; sorted-map assumes stacks are presented ascending, left to right
                    ;; a more accurate choice would be a collection that preserves insertion order
                    (into (sorted-map)))]
    stacks))

(defn parse-move-line [line]
  (let [pattern #"move (\d+) from (\d+) to (\d+)"]
    (when-let [match (re-matches pattern line)]
      {:quantity (->int (get match 1))
       :from     (->int (get match 2))
       :to       (->int (get match 3))})))

(defn parse-stack-state [lines]
  (let [[starting-state-input steps-input] (util/split-on-newline lines)
        stacks (parse-stacks starting-state-input)
        instructions (map parse-move-line steps-input)]
    stacks
    {:stacks       stacks
     :instructions instructions}))

(defn p1-step-fn [state {:keys [quantity from to] :as _instruction}]
  (let [from-stack (get state from)
        to-stack (get state to)
        subjects (reverse (take-last quantity from-stack))
        from-stack (drop-last quantity from-stack)
        to-stack (concat to-stack subjects)]
    (-> state
        (assoc from from-stack)
        (assoc to to-stack))))

(defn p2-step-fn [state {:keys [quantity from to] :as _instruction}]
  (let [from-stack (get state from)
        to-stack (get state to)
        subjects (take-last quantity from-stack)            ;; note: removed call to reverse from p1-step-fn
        from-stack (drop-last quantity from-stack)
        to-stack (concat to-stack subjects)]
    (-> state
        (assoc from from-stack)
        (assoc to to-stack))))

(comment

  (let [{:keys [stacks instructions]} (parse-stack-state (slurp input-path))]
    (->> instructions
         (reduce p1-step-fn stacks)
         (vals)
         (map #(last %))
         (apply str)))
  ;; SPFMVDTZT

  (let [{:keys [stacks instructions]} (parse-stack-state (slurp input-path))]
    (->> instructions
         (reduce p2-step-fn stacks)
         (vals)
         (map #(last %))
         (apply str)))
  ;;ZFSJBPRFP

  )