(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [aoc.util :refer [->int] :as util]))

(def input-path "resources/day07.txt")

(def sample-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(s/def ::command #{:ls :cd})
(s/def ::arg string?)
(s/def ::args (s/* ::arg))
(s/def ::command-line (s/keys :req [::command ::args]))

(s/def ::dir-name string?)
(s/def ::dir-line (s/keys :req [::dir-name]))

(s/def ::file-size int?)
(s/def ::file-name string?)
(s/def ::file-line (s/keys :req [::file-size ::file-name]))

(s/def ::input-type #{:command :dir :file})
(s/def ::input (s/keys :req [::input-type] :opt [::command-line ::dir-line ::file-line]))

(defn parse-input-line [line]
  (let [command-line-pattern #"\$ (?<command>\w+)(?<args>( ([a-zA-Z0-9]+|\.\.|/))*)"
        dir-line-pattern #"dir (?<name>[a-zA-Z]+)"
        file-line-pattern #"(?<size>\d+) (?<name>[a-zA-Z\.]+)"]
    (if-let [{:strs [command args] :as _command-groups} (util/re-get-named-group command-line-pattern line "command" "args")]
      {::input-type   :command
       ::command-line {::command (keyword command)
                       ::args    (util/split args #" " :remove-empty true)}}
      (if-let [{:strs [name] :as _dirs-groups} (util/re-get-named-group dir-line-pattern line "name")]
        {::input-type :dir
         ::dir-line   {::dir-name name}}
        (when-let [{:strs [size name] :as _file-groups} (util/re-get-named-group file-line-pattern line "size" "name")]
          {::input-type :file
           ::file-line  {::file-size (->int size)
                         ::file-name name}})))))

(defn process-cd [args state remaining]
  (let [{:keys [cd a-list]} state
        arg (first args)
        next-a-list (if (and (= arg "/") (not (contains? a-list "/")))
                      (assoc a-list "/" {:type     :dir
                                         :name     "/"
                                         :children #{}})
                      a-list)
        next-dir (if (= arg "..")
                   (if (empty? cd)
                     cd
                     (pop cd))
                   (conj cd arg))
        next-state (-> state
                       (assoc :cd next-dir)
                       (assoc :a-list next-a-list))]
    {:state  next-state
     :inputs remaining}))

(defn process-ls [_args state remaining]
  {:state  state
   :inputs remaining})

(defn process-command-input [state input remaining]
  (let [command-line (::command-line input)
        {:aoc.day07/keys [command args]} command-line]
    (cond
      (= command :cd) (process-cd args state remaining)
      (= command :ls) (process-ls args state remaining)
      :else (throw (ex-info (format "unknown command type: %s" command) {})))))

(defn cd->fqn
  "
  return the string representation of the fqn for 'cd' where 'cd' is a vector of string tokens,
  that combined from left to right produce an absolute path.

  for example:
  ['/' 'home' 'james'] => '/home/james'
  "
  [cd]
  (let [fqn (str (first cd) (str/join "/" (vec (drop 1 cd))))]
    fqn))

(defn process-dir-input [state input remaining]
  (let [{:keys [cd a-list]} state
        dir-line (::dir-line input)
        {:aoc.day07/keys [dir-name]} dir-line
        parent-fqn (cd->fqn cd)
        dir-fqn (str (first cd) (str/join "/" (conj (vec (drop 1 cd)) dir-name)))
        next-a-list (if (contains? a-list dir-fqn)
                      a-list
                      (assoc a-list dir-fqn {:type     :dir
                                             :name     dir-fqn
                                             :children #{}}))
        next-a-list (update-in next-a-list [parent-fqn :children] conj dir-fqn)
        next-state (assoc state :a-list next-a-list)]
    {:state  next-state
     :inputs remaining}))

(defn process-file-input [state input remaining]
  (let [{:keys [cd a-list]} state
        file-line (::file-line input)
        {:aoc.day07/keys [file-size file-name]} file-line
        parent-fqn (cd->fqn cd)
        file-fqn (str (first cd) (str/join "/" (conj (vec (drop 1 cd)) file-name)))
        next-a-list (if (contains? a-list file-fqn)
                      a-list
                      (assoc a-list file-fqn {:type :file
                                              :name file-fqn
                                              :size file-size}))
        next-a-list (update-in next-a-list [parent-fqn :children] conj file-fqn)
        next-state (assoc state :a-list next-a-list)]
    {:state  next-state
     :inputs remaining}))

(defn process-input [inputs]
  (loop [state {:cd [] :a-list nil}
         input inputs]
    (let [[head & tail] input
          conformed (s/conform ::input head)
          input-type (::input-type conformed)]
      (if (not= conformed :clojure.spec.alpha/invalid)
        (cond
          (= :command input-type) (let [{:keys [state inputs]} (process-command-input state head tail)]
                                    (recur state inputs))

          (= :dir input-type) (let [{:keys [state inputs]} (process-dir-input state head tail)]
                                (recur state inputs))

          (= :file input-type) (let [{:keys [state inputs]} (process-file-input state head tail)]
                                 (recur state inputs))
          :else state)
        state))))

(defn calculate-sizes [tree node-name acc]
  (let [node (get tree node-name)
        node-type (:type node)]
    (if (= node-type :file)
      (assoc acc node-name (:size node))
      (let [children (:children node)
            accumulated (->> children
                             (reduce (fn [acc fqn-child]
                                       (calculate-sizes tree fqn-child acc)) acc)) ;; todo - cheating. not idiomatic recursion
            dir-size (->> children
                          (map (fn [fqn] (get accumulated fqn)))
                          (reduce +))]
        (assoc accumulated node-name dir-size)))))

(def small-directory-threshold 100000)

;; find all of the directories with a total size of at most 100000, then calculate the sum of their total sizes.
(defn process-file-system
  [file-system]
  (let [sizes (calculate-sizes file-system "/" {})
        directory-sizes (->> file-system
                             (keep (fn [[fqn node]]
                                     (when (= (:type node) :dir)
                                       fqn)))
                             (map (fn [fqn] (get sizes fqn))))
        small-directory-sizes (->> directory-sizes
                                   (filter #(< % small-directory-threshold)))]
    {:sizes                 sizes
     :directory-sizes       directory-sizes
     :small-directory-total (reduce + small-directory-sizes)}))

(defn input->adjacency-list [s]
  (let [structured-input (->> (str/split-lines s)
                              (map parse-input-line))
        {adjacency-list :a-list :as _state} (process-input structured-input)]
    adjacency-list))

(def total-disk-space 70000000)
(def update-required-disk-space 30000000)

(comment

  ;;; part 1

  ;; 1477771
  (let [file-system (input->adjacency-list sample-input)
        file-system (input->adjacency-list (slurp input-path))
        file-system-stats (process-file-system file-system)]
    file-system-stats)

  ;;; part 2

  ;; 3579501
  (let [file-system (input->adjacency-list sample-input)
        file-system (input->adjacency-list (slurp input-path))
        file-system-stats (process-file-system file-system)
        used-space (get-in file-system-stats [:sizes "/"])
        unused-space (- total-disk-space used-space)
        required-remaining-space (- update-required-disk-space unused-space)
        next-smallest-directory-size (->> (:directory-sizes file-system-stats)
                                          (sort)
                                          (some #(and (> % required-remaining-space) %)))]
    ;; find minimally-sized directory whose size is > required-remaining-space
    next-smallest-directory-size)

  )
