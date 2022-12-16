(ns aoc.day11
  (:require [clojure.string :as str]
            [aoc.util :refer [->int] :as util]))

(def input-path "resources/day11.txt")

(def sample-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn parse-monkey-id [monkey-id-line]
  (let [monkey-id-pattern #"Monkey (\d+):"]
    (when-let [match (re-matches monkey-id-pattern monkey-id-line)]
      (get match 1))))

(defn parse-starting-items [starting-items-line]
  (let [starting-items-pattern #"\s*Starting items: (\d+(, \d+)*)"]
    (when-let [match (re-matches starting-items-pattern starting-items-line)]
      (as-> (get match 1) $
            (str/split $ #", ")
            (map util/->int $)
            (apply list $)))))

(defn parse-operation [operation-line]
  (let [operation-pattern #"\s*Operation: new = old ([+*]) (\w+)"]
    (when-let [match (re-matches operation-pattern operation-line)]
      (let [operator-token (get match 1)
            operator (cond
                       (= "+" operator-token) :+
                       (= "*" operator-token) :*
                       :else (throw (ex-info (format "unknown operator: %s" operator-token) {:token operator-token})))
            operand-token (get match 2)
            operand (if (= "old" operand-token)
                      {:type :symbol :value :old}
                      {:type :scalar :value (util/->int operand-token)})]
        {:operator operator
         :operand  operand}))))

(defn parse-conditional [[test-line if-line else-line :as _conditional-lines]]
  (let [test-line-pattern #"\s*Test: divisible by (\d+)"
        if-line-pattern #"\s*If true: throw to monkey (\d+)"
        else-line-pattern #"\s*If false: throw to monkey (\d+)"
        match-test (re-matches test-line-pattern test-line)
        match-if (re-matches if-line-pattern if-line)
        match-else (re-matches else-line-pattern else-line)]
    (when (every? #(not (nil? %)) [match-test match-if match-else])
      (let [test-arg (->int (get match-test 1))
            if-monkey (get match-if 1)
            else-monkey (get match-else 1)]
        {:test-arg    test-arg
         :if-monkey   if-monkey
         :else-monkey else-monkey}))))

(defn parse-monkey [lines]
  (let [[monkey-id-line starting-items-line operation-line & test-condition-lines] lines
        monkey-id (parse-monkey-id monkey-id-line)
        starting-items (parse-starting-items starting-items-line)
        operation (parse-operation operation-line)
        conditional (parse-conditional test-condition-lines)]
    {:monkey-id       monkey-id
     :items           starting-items
     :operation       operation
     :conditional     conditional
     :inspected-items []}))

(defn parse-input [s]
  (->> (util/split-on-newline s)
       (map parse-monkey)))

(def worry-level-map {[:* :scalar] (fn [worry-level operand-value] (* worry-level operand-value))
                      [:* :symbol] (fn [worry-level _worry-level] (* worry-level worry-level))
                      [:+ :scalar] (fn [worry-level operand-value] (+ worry-level operand-value))
                      [:+ :symbol] (fn [worry-level _worry-level] (+ worry-level worry-level))})

(defn next-worry-level
  [worry-level {:keys [operator operand] :as _operation}]
  (let [lookup-key [operator (:type operand)]
        worry-level-fn (get worry-level-map lookup-key)
        next (worry-level-fn worry-level (:value operand))]
    next))

(defn next-monkey [worry-level {:keys [test-arg if-monkey else-monkey] :as _conditional}]
  (if (zero? (mod worry-level test-arg))
    if-monkey
    else-monkey))

(defn process-round-p1 [state divisor]
  (->> (keys state)
       (reduce (fn [state monkey-id]
                 ;; When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list.
                 ;; A monkey that starts a round with no items could end up inspecting and throwing many items by the time its turn comes around.
                 ;; If a monkey is holding no items at the start of its turn, its turn ends.

                 ;;   Monkey inspects an item with a worry level of 79.
                 ;;   Worry level is multiplied by 19 to 1501.
                 ;;   Monkey gets bored with item. Worry level is divided by 3 to 500.
                 ;;   Current worry level is not divisible by 23.
                 ;;   Item with worry level 500 is thrown to monkey 3.

                 (loop [state state]
                   (let [[head & _tail] (get-in state [monkey-id :items])]
                     (if (nil? head)
                       state
                       (let [operation (get-in state [monkey-id :operation])
                             worry-level' (next-worry-level head operation)
                             worry-level' (quot worry-level' divisor)
                             conditional (get-in state [monkey-id :conditional])
                             destination-monkey (next-monkey worry-level' conditional)]

                         ;; modify current monkey's inspected-items: conj inspected-worry-level
                         ;; modify current monkey's items list: pop
                         ;; modify destination monkey's items list: (concat xs [worry-level'])

                         ;; recur state
                         (recur (-> state
                                    (update-in [monkey-id :inspected-items] conj head)
                                    (update-in [monkey-id :items] pop)
                                    (update-in [destination-monkey :items] (fn [items x] (apply list (concat items [x]))) worry-level')))))))
                 ) state)))

(defn process-round-p2 [state divisor]
  (->> (keys state)
       (reduce (fn [state monkey-id]
                 (loop [state state]
                   (let [[head & _tail] (get-in state [monkey-id :items])]
                     (if (nil? head)
                       state
                       (let [operation (get-in state [monkey-id :operation])
                             worry-level' (next-worry-level head operation)
                             worry-level' (mod worry-level' divisor) ;; wrap-around
                             conditional (get-in state [monkey-id :conditional])
                             destination-monkey (next-monkey worry-level' conditional)]
                         (recur (-> state
                                    (update-in [monkey-id :inspected-items] conj head)
                                    (update-in [monkey-id :items] pop)
                                    (update-in [destination-monkey :items] (fn [items x] (apply list (concat items [x]))) worry-level')))))))
                 ) state)))

(def part1-denominator 3)
(def part1-rounds 20)
(def part2-rounds 10000)

(comment

  ;;; Part 1

  ;; you're going to have to focus on the two most active monkeys if you want any hope of getting your stuff back.
  ;; Count the total number of times each monkey inspects items over 20 rounds

  ;; 58786
  (let [starting-state (as-> (slurp input-path) $
                             (parse-input $)
                             (apply util/index-by :monkey-id $))
        denominator part1-denominator]
    (as-> (range part1-rounds) $
          (reduce (fn [state _round] (process-round-p1 state denominator)) starting-state $)
          (map (fn [[_monkey-id monkey-state]] (-> monkey-state (get-in [:inspected-items]) (count))) $)
          (sort-by identity > $)
          (take 2 $)
          (reduce * $)))

  ;; Part 2

  (let [starting-state (as-> (slurp input-path) $
                             (parse-input $)
                             (apply util/index-by :monkey-id $))
        ;; divide by product of all divisible args
        denominator (->> starting-state
                         (map (fn [[_id monkey]] (get-in monkey [:conditional :test-arg])))
                         (reduce *))]
    (as-> (range part2-rounds) $
          (reduce (fn [state _round] (process-round-p2 state denominator)) starting-state $)
          (map (fn [[_monkey-id monkey-state]] (-> monkey-state (get-in [:inspected-items]) (count))) $)
          (sort-by identity > $)
          (take 2 $)
          (reduce * $)))

  )



