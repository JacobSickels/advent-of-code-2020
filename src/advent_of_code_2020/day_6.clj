(ns advent-of-code-2020.day-6
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]))

(defn day-6 []
  (let [data (core/read-file "resources/2020-6.txt")
        partition (partition-by #(= "" %) data)
        removed-empty (remove #(= (first %) "") partition)
        answers (map #(apply str %) removed-empty)
        characters (map set answers)]
    (reduce + (map count characters))))

(defn day-6-2 []
  (let [data (core/read-file "resources/2020-6.txt")
        partition (partition-by #(= "" %) data)
        removed-empty (remove #(= (first %) "") partition)
        answer-sets (map #(map set %) removed-empty)]
    (reduce + (map count (map #(apply set/intersection %) answer-sets)))))