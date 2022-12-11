(ns aoc.day08
  (:require [clojure.string :as str]
            [aoc.util :refer [->int] :as util]))

(def input-path "resources/day08.txt")

(def sample-input "30373
25512
65332
33549
35390")

(defn visible-coords [grid coords]
  (->> coords
       (reduce (fn [{:keys [_visible max] :as acc} [row col]]
                 (let [value (get-in grid [row col])]
                   ;; All of the trees around the edge of the grid are visible
                   ;; A tree is visible if all of the other trees between it and an edge of the grid are shorter than it.
                   (if (or (nil? max) (> value max))
                     (-> acc
                         (update :visible (fn [xs x] (conj xs x)) [row col])
                         (assoc :max value))
                     acc)))
               {:visible #{}
                :max     nil})
       (:visible)))

(defn scenic-score [grid [row col :as _coord]]
  (let [num-rows (count grid)
        num-cols (count (first grid))
        value (get-in grid [row col])
        coords-left (->> (range (dec col) -1 -1)
                         (map #(vector row %))
                         (map (fn [[row col]] (get-in grid [row col])))
                         (util/take-until (fn [neighboring-value]
                                            (or (nil? neighboring-value)
                                                (<= value neighboring-value)))))
        coords-right (->> (range (inc col) num-cols 1)
                          (map #(vector row %))
                          (map (fn [[row col]] (get-in grid [row col])))
                          (util/take-until (fn [neighboring-value]
                                             (or (nil? neighboring-value)
                                                 (<= value neighboring-value)))))
        coords-up (->> (range (dec row) -1 -1)
                       (map #(vector % col))
                       (map (fn [[row col]] (get-in grid [row col])))
                       (util/take-until (fn [neighboring-value]
                                          (or (nil? neighboring-value)
                                              (<= value neighboring-value)))))
        coords-down (->> (range (inc row) num-rows 1)
                         (map #(vector % col))
                         (map (fn [[row col]] (get-in grid [row col])))
                         (util/take-until (fn [neighboring-value]
                                            (or (nil? neighboring-value)
                                                (<= value neighboring-value)))))]
    (->> (list coords-left coords-right coords-up coords-down)
         (map count)
         (reduce *))))

(comment

  ;; 1763
  (let [grid (as-> (slurp input-path) $
                   (str/split-lines $)
                   (mapv (fn [line] (mapv (comp ->int str) line)) $))
        ;; Only consider trees in the same row or column;
        left-to-right (util/coords-2d grid)
        l2r-rows (partition-by (fn [[row _col]] row) left-to-right)
        r2l-rows (map reverse l2r-rows)
        top-to-bottom (util/coords-2d-columnar grid)
        t2b-columns (partition-by (fn [[_row col]] col) top-to-bottom)
        b2t-columns (map reverse t2b-columns)]

    (->> (concat l2r-rows r2l-rows t2b-columns b2t-columns)
         (map #(visible-coords grid %))
         (reduce clojure.set/union)
         ;; how many trees are visible from outside the grid?
         (count)))

  ;; 671160
  (let [grid (as-> (slurp input-path) $
                   (str/split-lines $)
                   (mapv (fn [line] (mapv (comp ->int str) line)) $))
        coords (util/coords-2d grid)
        scenic-scores (map #(scenic-score grid %) coords)]
    (apply max scenic-scores))

  )

