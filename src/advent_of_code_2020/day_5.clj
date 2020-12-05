(ns advent-of-code-2020.day-5
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]
            [clojure.string :as str]))


(defn binary-search [col high-check low-check initial-values]
  (loop [collection col
         [low high] initial-values]
    (cond
      (high-check (first collection)) 
      (recur (rest collection) [(int (Math/ceil (/ (+ high low) 2))) high])
      (low-check (first collection))
      (recur (rest collection) [low (int (Math/floor (/ (+ high low) 2)))])
      (= high low) high)))

(defn find-seat-id [partition-string]
  (+ (* (binary-search (take 7 partition-string)
                       (fn [x] (= x \B))
                       (fn [x] (= x \F))
                       [0 127]) 8)
     (binary-search (take-last 3 partition-string)
                    (fn [x] (= x \R))
                    (fn [x] (= x \L))
                    [0 7])))

(defn day-5 []
  (let [data (core/read-file "resources/2020-5.txt")]
    (last (sort (map find-seat-id data)))))

(defn day-5-2 []
  (let [data (core/read-file "resources/2020-5.txt")]
    (first (set/difference (set (range 11 835)) ; 11 835 are the seat-id bounds from part 1
                           (set (sort (map find-seat-id data)))))))


; After figuring out I didn't need to do a binary search
(defn find-seat-id->binary [partition-string]
    (let [binary-string (str/replace partition-string #"F|B|R|L" {"F" "0" "B" "1" "R" "1" "L" "0"})]
      (Long/parseLong binary-string 2)))

(defn day-5->binary []
  (let [data (core/read-file "resources/2020-5.txt")]
    (last (sort (map find-seat-id->binary data)))))

(defn day-5-2->binary []
  (let [data (core/read-file "resources/2020-5.txt")]
    (first (set/difference (set (range 11 835)) ; 11 835 are the seat-id bounds from part 1
                           (set (sort (map find-seat-id->binary data)))))))