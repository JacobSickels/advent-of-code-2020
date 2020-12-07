(ns advent-of-code-2020.day-7
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as str]))

(def test-data ["shiny gold bags contain 2 dark red bags."
                "dark red bags contain 2 dark orange bags."
                "dark orange bags contain 2 dark yellow bags."
                "dark yellow bags contain 2 dark green bags."
                "dark green bags contain 2 dark blue bags."
                "dark blue bags contain 2 dark violet bags."
                "dark violet bags contain no other bags."])

(def test-data-2
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])


(defn bags-that-contain-bag [bags-map find-bag-type]
  (contains? (second bags-map) find-bag-type))

(defn format-data [data]
  (let [split-values (map #(str/split % #" contain ") data)
        secondary-groups (map (fn [[k v]] [k (str/split v #", ")]) split-values)
        secondary-formatting (map (fn [[k v]]
                                    [k (map #(let [d (str/split % #" ")]
                                               (if (= "no" (first d))
                                                 ["other" 0]
                                                 [(str/join " " (butlast (rest d)))
                                                  (Integer/parseInt (first d))]))
                                            v)])
                                  secondary-groups)
        secondary-maps (map (fn [[k v]] [k (into (sorted-map) v)]) secondary-formatting)
        initial-formatting (into (sorted-map)
                                 (map (fn [[k v]]
                                        [(str/join " " (butlast (str/split k #" "))) v])) secondary-maps)]
    initial-formatting))

(defn day-7 []
  (let [data (core/read-file "resources/2020-7.txt")
        formatted-data (format-data data)]
    (loop [acc []
           search ["shiny gold"]]
      (if (empty? search)
        (count (set acc))
        (let [found (filter #(bags-that-contain-bag % (first search)) formatted-data)]
          (recur (concat acc found) (concat (rest search) (map first found))))))))

(defn math-children [bags-map key]
  (let [children (get bags-map key)]
    (if (= (first (keys children)) "other") 
      0
      (reduce + 
              (map #(* (second %) (+ 1 (math-children bags-map (first %))))
                   children)))))

(defn day-7-2 []
  (let [data (core/read-file "resources/2020-7.txt")]
    (math-children (format-data data) "shiny gold")))