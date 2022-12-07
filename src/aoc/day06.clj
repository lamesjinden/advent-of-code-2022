(ns aoc.day06
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [aoc.util :refer [->int drop-nil] :as util]))

(def input-path "resources/day06.txt")

(def sample-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def sample-input2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(def sample-input3 "nppdvjthqldpwncqszvftbrmjlhg")
(def sample-input4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def sample-input5 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(def packet-marker-size 4)
(def message-marker-size 14)

(defn characters-before-marker [s marker-size]
  (let [marker (->> (vec s)
                    (partition marker-size 1)
                    (map-indexed (fn [index xs]
                                   {:index  index
                                    :window (into #{} xs)}))
                    (filter (fn [{:keys [window]}]
                              (= marker-size (count window))))
                    (first))]
    (+ marker-size (:index marker))))

(defn characters-before-packet-marker [s]
  (characters-before-marker s packet-marker-size))

(defn characters-before-message-marker [s]
  (characters-before-marker s message-marker-size))

(t/deftest packet-marker-tests
  (t/is (= (characters-before-packet-marker sample-input) 7))
  (t/is (= (characters-before-packet-marker sample-input2) 5))
  (t/is (= (characters-before-packet-marker sample-input3) 6))
  (t/is (= (characters-before-packet-marker sample-input4) 10))
  (t/is (= (characters-before-packet-marker sample-input5) 11)))

(t/deftest message-marker-tests
  (t/is (= (characters-before-message-marker sample-input) 19))
  (t/is (= (characters-before-message-marker sample-input2) 23))
  (t/is (= (characters-before-message-marker sample-input3) 23))
  (t/is (= (characters-before-message-marker sample-input4) 29))
  (t/is (= (characters-before-message-marker sample-input5) 26)))

(comment

  (t/run-tests)

  ;; 1848
  (characters-before-packet-marker (slurp input-path))

  ;; 2308
  (characters-before-message-marker (slurp input-path))

  )