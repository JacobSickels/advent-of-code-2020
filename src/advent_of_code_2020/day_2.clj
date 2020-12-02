(ns advent-of-code-2020.day-2
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as str]))

(defn follows-part-1-policy?
  [[policy password]]
  (let [[range find-char] (str/split policy #" ")
        character (first (seq (char-array find-char)))
        [lower higher] (map #(Integer/parseInt %) (str/split range #"-"))
        frequency (frequencies password)]
    (if (nil? (get frequency character))
      false
      (and (<= lower (get frequency character)) (>= higher (get frequency character))))))

(defn follows-part-2-policy?
  [[policy password]]
  (let [[range find-char] (str/split policy #" ")
        character (first (seq (char-array find-char)))
        [lower higher] (map #(Integer/parseInt %) (str/split range #"-"))
        char-lower (get password (dec lower))
        char-higher (get password (dec higher))]
    (core/xor (= character char-lower) (= character char-higher))))

(defn day-2
  []
  (let [data (core/read-file "resources/2020-2.txt")
        formatted (map #(str/split % #": ") data)]
    (get (frequencies (map follows-part-1-policy? formatted)) true)))

(defn day-2-2
  []
  (let [data (core/read-file "resources/2020-2.txt")
        formatted (map #(str/split % #": ") data)]
    (get (frequencies (map follows-part-2-policy? formatted)) true)))