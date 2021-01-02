(ns advent-of-code-2020.day-23
  (:require [clojure.string :as str]))

(def input [3 8 9 1 2 5 4 6 7])

; rotates list so number is at the first position
(defn rotate-list [list num]
  (let [indexof-num (str/index-of (apply str list) (str num))]
    (concat (drop indexof-num list) (take indexof-num list))))

(defn find-destination [chunk destination]
  (loop [d destination]
    (if (not (contains? (set chunk) d))
      d
      (let [num (if (= d 1) 9 (dec d))]
        (recur num)))))
      
      
  

(defn day-23 []
  (loop [l input
         pointer 0
         moves 0]
    (if (= moves 10)
      l
      (let [starting (nth l (mod pointer 9))
            rotated (rotate-list l starting)
            chunk (take 3 (rest rotated))
            removed-chunk (concat (take 1 rotated) (drop 4 rotated))
            destination (find-destination chunk (dec starting))]
        (println l chunk destination)
        (let [index-of-num (str/index-of (apply str removed-chunk) (str destination))
              reformed-chunk (concat (subvec (vec removed-chunk) 0 (inc index-of-num))
                                     (vec chunk)
                                     (subvec (vec removed-chunk) (inc index-of-num)))]
          (recur reformed-chunk  (inc pointer) (inc moves)))))))