(ns advent-of-code-2020.day-9
  (:require [clojure.math.combinatorics :as combo]
            [advent-of-code-2020.core :as core]))

(defn index-is-invalid? [input index preamble-length]
  (let [preamble (subvec input (- index preamble-length) index)
        valid-preamble (set (map #(apply + %) (combo/combinations preamble 2)))]
    [(nil? (valid-preamble (get input index))) (get input index) index]))

(defn day-9 []
  (let [data (vec (map #(bigint %) (core/read-file "resources/2020-9.txt")))
        preamble-length 25
        check-valid (map-indexed (fn [idx num] 
                                   (index-is-invalid? data (+ preamble-length idx) preamble-length)) 
                                 (drop preamble-length data))]
    (filter #(true? (first %)) check-valid)))

; Answer from part 1 - 675280050, index 619
(defn day-9-2 []
  (let [data (vec (map #(bigint %) (core/read-file "resources/2020-9.txt")))
        data-set (subvec data 25 619)
        find-number 675280050]
    (loop [begin 0
           end 1]
      (let [find-range (subvec data-set begin end)
            sum-range (apply + find-range)]
        (if (= sum-range find-number)
          (+ (apply min find-range) (apply max find-range))
          (if (> sum-range find-number)
            (recur (inc begin) (+ 2 begin))
            (recur begin (inc end))))))))
  
  
         