(ns advent-of-code-2020.day-3
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as str]))

(defn translate [[x y] width] [(mod x width) y])

(defn find-trees
  [slopeX slopeY]
  (let [data (core/read-file "resources/2020-3.txt")
        formatted (vec (map #(str/split % #"") data))
        width (count (first data))
        height (count data)]
    (loop [[x y] [0 0]
           locations []]
      (if (>= y height)
        (get (frequencies locations) "#")
        (recur (translate [(+ x slopeX) (+ y slopeY)] width)
               (conj locations (get (get formatted y) x)))))))

(defn day-3 []
  (find-trees 3 1))

(defn day-3-2
      []
      (* (find-trees 1 1)
         (find-trees 3 1)
         (find-trees 5 1)
         (find-trees 7 1)
         (find-trees 1 2)))

