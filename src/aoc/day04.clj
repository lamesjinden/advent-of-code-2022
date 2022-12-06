(ns aoc.day04
  (:require [clojure.string :as str]
            [clojure.set :as clj-set]
            [aoc.util :refer [->int]]))

(def input-path "resources/day04.txt")

(def sample-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")

(defn range-token->set [token]
  (let [[start end] (str/split token #"-")
        range (range (->int start) (inc (->int end)))]
    (apply hash-set range)))

(defn line->ranges [line]
  (let [[a b] (str/split line #",")
        set-a (range-token->set a)
        set-b (range-token->set b)]
    {:elf-1 set-a
     :elf-2 set-b}))

(defn fully-contained?
  "tests if s1 contains s2"
  [s1 s2]
  (let [intersection (clj-set/intersection s1 s2)]
    (= (count intersection) (count s2))))

(defn partially-contained?
  "tests if s1 and s2 contain any overlapping elements"
  [s1 s2]
  (let [difference (clj-set/difference s1 s2)]
    (< (count difference) (count s1))))

(comment

  (->> (slurp input-path)
       (str/split-lines)
       (map #(line->ranges %))
       (map (fn [{:keys [elf-1 elf-2]}]
              (or (fully-contained? elf-1 elf-2) (fully-contained? elf-2 elf-1))))
       (filter true?)
       (count))

  (->> (slurp input-path)
       (str/split-lines)
       (map #(line->ranges %))
       (map (fn [{:keys [elf-1 elf-2]}]
              (or (partially-contained? elf-1 elf-2) (partially-contained? elf-2 elf-1))))
       (filter true?)
       (count))

  )
