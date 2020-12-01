(ns advent-of-code-2020.day-1
  (:require [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(defn extended-find
  [find-number collection]
  (loop [col collection
         p1 0
         p2 1]
    (if (empty? col)
      nil
      (if (nil? (get col p2))
        (recur (vec (rest col)) 0 1)
        (if (nil? (get col p1))
          nil
          (if (= find-number (+ (get col p1) (get col p2)))
            [(get col p1) (get col p2)]
            (recur col p1 (inc p2))))))))

(defn day-1 
  []
  (let [data (core/read-file "resources/2020-1.txt")
        integers (map #(Integer/parseInt %) data)]
    (extended-find 2020 (vec integers))))

(defn day-1-2
  []
  "extended-find here is the function that searches the whole collection to find
  the two numbers that add up to find-number. In second part of day 2, we can reuse this
  function to find the two numbers that add up to 2020 - pointer, where pointer is the number
  in the collection we are trying to find matches for."
  (let [data (core/read-file "resources/2020-1.txt")
        integers (map #(Integer/parseInt %) data)]
    (loop [col (vec integers)
           p1 0]
      (let [ans (extended-find (- 2020 (get col p1)) col)]
        (if (nil? ans)
          (recur (vec col) (inc p1))
          (conj ans (get col p1)))))))
  
        
