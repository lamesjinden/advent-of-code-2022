(ns aoc.day10
  (:require [clojure.string :as str]
            [aoc.util :refer [->int] :as util]
            [name.stadig.deque :as dq]))

(def input-path "resources/day10.txt")

(def sample-input "noop\naddx 3\naddx -5")

(def sample-input2 "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n")

;;The CPU has a single register, X, which starts with the value 1. It supports only two instructions:
;;
;;   addx V takes two cycles to complete. After two cycles, the X register is increased by the value V. (V can be negative.)
;;   noop takes one cycle to complete. It has no other effect.

(def initial-cpu-state {:register-x 1})

(defn parse-instruction [line]
  (let [noop-pattern #"^noop$"
        addx-pattern #"^addx (-?\d+)$"]
    (if-let [_noop-match (re-matches noop-pattern line)]
      {:type             :noop
       :cycles-remaining 1}
      (if-let [addx-match (re-matches addx-pattern line)]
        {:type             :addx
         :arg              (->int (get addx-match 1))
         :cycles-remaining 2}
        (throw (ex-info "unmatched instruction" {:input line}))))))

(defn parse-instructions [input]
  (->> input
       (str/split-lines)
       (map parse-instruction)))

(defn step-noop [cpu-state]
  (let [next-state (update-in cpu-state [:instructions] pop)]
    next-state))

(defn step-addx [cpu-state {:keys [arg cycles-remaining] :as _instruction}]
  (if (> cycles-remaining 1)
    (-> cpu-state
        (update-in [:instructions] (fn [q]
                                     (let [next-head (-> q (peek) (update :cycles-remaining dec))]
                                       (-> q
                                           (pop)
                                           (conj next-head))))))
    (-> cpu-state
        (update-in [:register-x] + arg)
        (update-in [:instructions] pop))))

(defn step-cpu-state [{:keys [instructions] :as cpu-state} cycle]
  (let [cpu-state (assoc cpu-state :cycle cycle)
        {:keys [type] :as current-instruction} (peek instructions)]
    (cond
      (nil? current-instruction) nil
      (= type :noop) (step-noop cpu-state)
      (= type :addx) (step-addx cpu-state current-instruction)
      :else (throw (ex-info "unknown instruction type" {:type type})))))

(defn- step-fn
  "
  delegates to step-cpu-state for the next state,
  keeping all intermediate states produced along the way
  "
  [{:keys [state] :as acc} cycle]
  (let [next-state (step-cpu-state state cycle)]
    (if (nil? next-state)
      (reduced acc)
      (-> acc
          (assoc-in [:state] next-state)
          ;; note - intermediate state captures state coming into (aka 'during') the cycle; NOT the result
          (assoc-in [:intermediate-states cycle] state)))))

(defn process-instructions [cpu-state instructions]
  (let [instruction-queue (apply conj dq/empty-deque (reverse instructions))
        state (assoc cpu-state :instructions instruction-queue)
        seed {:state               state
              :intermediate-states (sorted-map)}]
    (->> (range)
         (map inc)
         (reduce step-fn seed))))

(defn sprite-visible [[cycle register-x]]

  ;; If the sprite is positioned such that one of its three pixels is the pixel currently being drawn,
  ;; the screen produces a lit pixel (#); otherwise,
  ;; the screen leaves the pixel dark (.)

  (let [line-index (mod cycle 40)
        line-index-left (dec line-index)
        line-index-right (inc line-index)
        sprite-indices #{line-index-left line-index line-index-right}
        position register-x]
    (contains? sprite-indices position)))

(defn render-line [cycles]
  (->> cycles
       (mapv #(if (sprite-visible %) "#" "."))))

(defn print-lines [lines]
  (doseq [line lines]
    (println (apply str line))))

(comment
  ;; 14360
  (let [instructions (->> (slurp input-path)
                          (parse-instructions))
        interesting-cycles #{20 60 100 140 180 220}
        aggregate-state (process-instructions initial-cpu-state instructions)
        intermediate-states (:intermediate-states aggregate-state)]
    (->> intermediate-states
         (map (fn [[cycle state]] [cycle (:register-x state)]))
         (filter (fn [[cycle _register]] (contains? interesting-cycles cycle)))
         (map (fn [[cycle register]] (* cycle register)))
         (reduce +)))

  ;; "BGKAEREZ"
  (let [instructions (->> (slurp input-path)
                          (parse-instructions))
        ;; 20th, 60th, 100th, 140th, 180th, and 220th
        interesting-cycles #{20 60 100 140 180 220}
        aggregate-state (process-instructions initial-cpu-state instructions)
        intermediate-states (:intermediate-states aggregate-state)
        rendered-lines (->> intermediate-states
                            (map (fn [[cycle state]] [(dec cycle) (:register-x state)]))
                            (partition 40)
                            (map render-line))]
    (print-lines rendered-lines))

  )
