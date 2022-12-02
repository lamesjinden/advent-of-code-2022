(ns aoc.day01
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def day01-input-path "resources/day01.txt")

(def sample-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(comment

  (->> (slurp day01-input-path)
       (util/split-on-newline)
       (map (fn [ss]
              (->> ss
                   (map (fn [s] (Integer/parseInt s)))
                   (reduce +))))
       (sort)
       (last)) ; 71502

  (->> (slurp day01-input-path)
       (util/split-on-newline)
       (map (fn [ss]
              (->> ss
                   (map (fn [s] (Integer/parseInt s)))
                   (reduce +))))
       (sort)
       (reverse)
       (take 3)
       (reduce +)) ; 208191

  )