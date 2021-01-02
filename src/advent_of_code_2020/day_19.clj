(ns advent-of-code-2020.day-19
  (:require [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(def test-rules ["0: 4 1 5"
                 "1: 2 3 | 3 2"
                 "2: 4 4 | 5 5"
                 "3: 4 5 | 5 4"
                 "4: a"
                 "5: b"])

(def test-rules-2 {"0" "1 2"
                   "1" "a"
                   "2" "1 3 | 3 1"
                   "3" "b"})

(defn format-data [input]
  (into (sorted-map) (map #(str/split % #": ") input)))

(defn parse-rules [start rules]
  (let [rule (get rules start)]
    (if (vector? rule)
      (if (vector? (first rule))
        (map #(map (fn [r] (parse-rules r rules)) %) rule)
        (map #(parse-rules % rules) rule))
      rule)))


(defn parse-rules-regex [start rules]
  (println start)
  (loop [l (get rules start)]
    (let [found (re-find #"\d+" l)]
      (if (nil? found)
        l
        (let [parsed (parse-rules-regex found rules)]
          (if (str/includes? parsed "|")
            (recur (str/replace l found (str "(" parsed ")")))
            (recur (str/replace l found parsed))))))))

(defn day-19 []
  (let [
        rule-data (format-data (core/read-file "resources/2020-19-rules.txt"))
        data (core/read-file "resources/2020-19-data.txt")
        ;rule-data (format-data test-rules)
        ;data ["ababbb" "bababa" "abbbab" "aaabbb" "aaaabbb"]
        parsed-rule-str (parse-rules-regex "0" rule-data)
        parsed-rule-string (str/replace parsed-rule-str #" " "")
        parsed-rule-regex (re-pattern parsed-rule-string)]
    (count (filter #(not (nil? (re-matches parsed-rule-regex %))) data))))