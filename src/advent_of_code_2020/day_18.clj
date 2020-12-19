(ns advent-of-code-2020.day-18
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(defn parse-line [input-line]
  (let [line (map edn/read-string (str/split input-line #" "))
        numbers (take-nth 2 line)
        functions (take-nth 2 (rest line))]
    (loop [nums numbers
           funcs functions]
      (if (empty? funcs)
        (first nums)
        (let [value (reduce (case (first funcs)
                              + +
                              * *)
                            (take 2 nums))]
          (recur (concat [value] (drop 2 nums)) (rest funcs)))))))

(defn get-between-brackets [line]
  (loop [l line]
    (let [found (re-find #"\(([^()]*)\)" l)]
      (if (nil? found)
       (parse-line l)
       (let [parsed (parse-line (second found))]
         (recur (str/replace l (first found) (str parsed))))))))

(defn day-18 []
  (let [data (core/read-file "resources/2020-18.txt")]
    (reduce + (map get-between-brackets data))))

(defn parse-line-not-pemdas [input-line]
  (loop [l input-line]
    (let [found (re-find #"\d+ \+ \d+" l)]
      (if (nil? found)
        (parse-line l)
        (let [parsed (parse-line found)]
          (recur (str/replace l found (str parsed))))))))

(defn get-between-brackets-not-pemdas [line]
  (loop [l line]
    (let [found (re-find #"\(([^()]*)\)" l)]
      (if (nil? found)
        (parse-line-not-pemdas l)
        (let [parsed (parse-line-not-pemdas (second found))]
          (recur (str/replace l (first found) (str parsed))))))))

(defn day-18-2 []
  (let [data (core/read-file "resources/2020-18.txt")]
    (reduce + (map get-between-brackets-not-pemdas data))))
  