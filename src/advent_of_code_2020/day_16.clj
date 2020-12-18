(ns advent-of-code-2020.day-16
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn is-valid-for-ranges? [number [[l1 l2] [r1 r2]]]
  (or (<= l1 number l2) (<= r1 number r2)))

(defn validate-rules [number rules]
  [number (map (fn [[k v]] (list k (is-valid-for-ranges? number v))) rules)])

(defn not-valid-for-any? [[number validated-rules]]
  [number (every? false? (map second validated-rules))])

(def rules {:wagon              [[43 101] [118 951]],
            :arrival-station    [[47 306] [323 973]],
            :departure-platform [[46 121] [138 965]],
            :departure-time     [[32 64] [79 952]],
            :zone               [[45 844] [858 970]],
            :arrival-location   [[49 879] [905 968]],
            :departure-station  [[45 278] [294 974]],
            :type               [[33 205] [218 965]],
            :duration           [[47 414] [423 950]],
            :arrival-platform   [[46 823] [834 971]],
            :route              [[42 779] [799 970]],
            :train              [[25 914] [926 958]],
            :departure-track    [[38 149] [173 949]],
            :departure-location [[45 535] [550 961]],
            :class              [[40 350] [372 965]],
            :departure-date     [[34 223] [248 957]],
            :price              [[45 507] [526 956]],
            :arrival-track      [[30 464] [486 963]],
            :row                [[26 865] [872 955]],
            :seat               [[43 724] [739 970]]})

(def my-ticket [173, 191, 61, 199, 101, 179, 257, 79, 193, 223, 139, 97, 83, 197, 251, 53, 89, 149, 181, 59])
(def other-tickets (map #(map (fn [x] (Integer/parseInt x)) %)
                        (map (fn [l] (str/split l #","))
                             (core/read-file "resources/2020-16-other-tickets.txt"))))



(defn find-invalid-other-tickets []
  (reduce + (map (fn [ticket] (ffirst ticket))
              (filter (fn [invalid-ticket] 
                        (not (empty? invalid-ticket)))
                (map (fn [validated-ticket] 
                       (filter #(true? (second %)) validated-ticket)) 
                  (map (fn [ticket] 
                         (map #(not-valid-for-any? (validate-rules % rules)) 
                              ticket)) 
                       other-tickets))))))


(defn fields-for-number [number rules]
  (map first
       (filter #(true? (second %))
         (map (fn [[k v]] 
                (list k (is-valid-for-ranges? number v))) rules))))
 

(defn valid-tickets []
  (map (fn [l] (map first l)) 
    (filter (fn [validated-ticket] (every? false? (map second validated-ticket)))
      (map (fn [ticket]
             (map #(not-valid-for-any? (validate-rules % rules))
                  ticket))
           other-tickets))))
  
(defn column-could-be-rule [column-num]
  (apply set/intersection
         (map #(set (fields-for-number % rules))
              (map (fn [col] (nth col column-num )) (valid-tickets)))))

(defn columns-could-be-rules []
  (map column-could-be-rule (range 0 20)))

(defn find-rule-columns []
  (loop [could-be-rules (columns-could-be-rules)
         found []
         acc []]
    (if (= (count found) 20)
       (sort-by first acc)
      (let [set-indexed (map-indexed (fn [idx s] [idx s]) could-be-rules)
            [found-index found-key] (first (filter (fn [[idx set]] (= 1 (count set))) set-indexed))]
        (recur (map #(apply (partial disj %) (conj found (first found-key))) could-be-rules) 
               (conj found (first found-key)) 
               (conj acc [found-index (first found-key)]))))))
      
(defn day-16-2 []
  (reduce *
          (map (fn [x] (get my-ticket x))
               (map first 
                    (filter #(contains? #{:departure-track :departure-date :departure-platform 
                                          :departure-time :departure-station :departure-location} (second %))
                            (find-rule-columns))))))