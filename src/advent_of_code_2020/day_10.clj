(ns advent-of-code-2020.day-10
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn is-valid-config? [col]
  (let [jolts (set (map #(apply - %) (partition 2 1 col)))]
    (set/subset? jolts #{1 2 3})))

(defn day-10 []
  (let [data (core/read-file "resources/2020-10.txt")
        sorted (sort (concat (map #(Integer/parseInt %) data) [0]))
        all (reverse (concat sorted [(+ 3 (last sorted))]))]
    (println sorted)
    (apply * (vals (frequencies (map #(apply - %) (partition 2 1 all)))))))

; start steal - I stole this from https://stackoverflow.com/questions/23207490/partition-a-seq-by-a-windowing-predicate-in-clojure
(defn partition-between [pred? coll]
  (let [switch (reductions not= true (map pred? coll (rest coll)))]
    (map (partial map first) (partition-by second (map list coll switch)))))
; end steal

(defn make-valid-combos [col]
  (let [min (apply min col)
        max (apply max col)]
    (filter #(let [elm-set (set %)]
               (and (set/subset? #{min max} elm-set)
                    (is-valid-config? %)))
            (remove (fn [l] (< (count l) 2))
                    (combo/subsets col)))))

(defn day-10-2 []
  (let [data (core/read-file "resources/2020-10.txt")
        sorted (sort (concat (map #(Integer/parseInt %) data) [0]))
        all (reverse (concat sorted [(+ 3 (last sorted))]))
        partitions (partition-between (fn [a b] (> (- a b) 2)) all)
        make-combos (map #(if (< (count %) 2)
                            %
                            (make-valid-combos %))
                         partitions)]
    (apply * (map count make-combos))))
    
    
    
    