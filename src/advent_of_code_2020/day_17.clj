(ns advent-of-code-2020.day-17
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code-2020.core :as core]
            [clojure.set :as set]))


(def input ["..#..#.."
            ".###..#."
            "#..##.#."
            "#.#.#.#."
            ".#..###."
            ".....#.."
            "#...####"
            "##....#."])

(def test-input [".#."
                 "..#"
                 "###"])

; A list of starting points that are active
(defn starting-points-3d [input]
  (map butlast
       (filter #(= (last %) \#)
               (partition 4 (flatten
                              (map-indexed
                                (fn [idy l]
                                  (map-indexed (fn [idx c] (list idx idy 0 c)) l))
                                (map seq input)))))))

(defn starting-points-4d [input]
  (map butlast
       (filter #(= (last %) \#)
               (partition 5 (flatten
                              (map-indexed
                                (fn [idy l]
                                  (map-indexed (fn [idx c] (list idx idy 0 0 c)) l))
                                (map seq input)))))))

(defn considered-space [active-points]
  (let [dimension (count (first active-points))
        space (map (fn [n] (map #(nth % n) active-points))
                   (range 0 dimension))
        max-dimensions (map #(apply max %) space)
        min-dimensions (map #(apply min %) space)
        considered-points (map #(range (dec %1) (+ 2 %2)) min-dimensions max-dimensions)]
    (apply combo/cartesian-product considered-points)))

(defn next-state [active-points]
  (let [space (considered-space active-points)]
    (loop [considered-points space
           acc []]
      (if (empty? considered-points)
        (sort-by last acc)
        (let [considered-point (first considered-points)
              is-active? (contains? (set active-points) considered-point)
              surrounding-points (core/points-around considered-point)
              number-active-points-around (count (set/intersection (set active-points) (set surrounding-points)))]
          (cond 
            (and is-active? (contains? #{2 3} number-active-points-around))
            (recur (rest considered-points) (conj acc considered-point))
            (and (not is-active?) (= 3 number-active-points-around))
            (recur (rest considered-points) (conj acc considered-point))
            :else (recur (rest considered-points) acc)))))))
        
(defn loop-conway-3d []
  (loop [points (starting-points-3d input)
         iteration 0]
    (if (= iteration 6)
      (count points)
      (recur (next-state points) (inc iteration)))))

(defn loop-conway-4d []
  (loop [points (starting-points-4d input)
         iteration 0]
    (if (= iteration 6)
      (count points)
      (recur (next-state points) (inc iteration)))))
        
  