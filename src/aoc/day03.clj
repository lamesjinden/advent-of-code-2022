(ns aoc.day03
  (:require [clojure.string :as str]
            [clojure.set :as clj-set]
            [aoc.util :as util]))

(def day03-input-path "resources/day03.txt")

(def sample-input "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")

(def item->priority (let [a (int \a)
                          lowercase-priorities (->> (range a (inc (int \z)))
                                                    (mapcat #(vector (char %)
                                                                     (inc (- % a))))
                                                    (apply hash-map))

                          range-count (inc (- (int \z) a))
                          A (int \A)
                          uppercase-priorities (->> (range A (inc (int \Z)))
                                                    (mapcat #(vector
                                                               (char %)
                                                               (+ (inc (- % A)) range-count)))
                                                    (apply hash-map))]
                      (merge lowercase-priorities uppercase-priorities)))

(defn half [coll]
  (let [length (count coll)
        half (int (/ length 2))]
    (partition half coll)))

(comment

  (->> (slurp day03-input-path)
       (str/split-lines)
       (map half)
       (map (fn [[left right]]
              (clj-set/intersection (apply hash-set left) (apply hash-set right))))
       (map (fn [[first & _rest]] first))
       (map #(get item->priority %))
       (reduce +))

  (->> (slurp day03-input-path)
       (str/split-lines)
       (partition 3)
       (map (fn [[x y z]]
              (clj-set/intersection (apply hash-set x) (apply hash-set y) (apply hash-set z))))
       (map (fn [[first & _rest]] first))
       (map #(get item->priority %))
       (reduce +))

  )

