(ns advent-of-code-2020.day-8
  (:require [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(defn is-infinite? [input]
  (let [formatted (map (fn [text]
                         (let [[op arg] (str/split text #" ")]
                           [op (first arg) (Integer/parseInt (apply str (rest arg))) 0]))
                       input)]
    (loop [acc 0
           pointer 0
           instructions (vec formatted)]
      (if (nil? (get instructions pointer))
        [false acc]
        (let [instruction (get instructions pointer)
              [op change value call-count] instruction]
          (if (= call-count 1)
            [true acc]
            (cond 
              (= op "acc") (recur ((get {\+ + \- -} change) acc value) (inc pointer) (vec (assoc instructions pointer [op change value (inc call-count)])))
              (= op "jmp") (recur acc ((get {\+ + \- -} change) pointer value) (vec (assoc instructions pointer [op change value (inc call-count)])))
              (= op "nop") (recur acc (inc pointer) (vec (assoc instructions pointer [op change value (inc call-count)]))))))))))

(defn day-8 []
  (let [data (core/read-file "resources/2020-8.txt")
        response (is-infinite? data)]
    (first response)))

(defn day-8-2 []
  (let [data (core/read-file "resources/2020-8.txt")
        jumps (set (filter #(= "jmp" (first (str/split (second %) #" "))) (map-indexed (fn [idx item] [idx item]) data)))]
    (loop [indexes jumps]
      (let [infinite? (is-infinite? (assoc data (first (first indexes)) 
                                                (str/replace (second (first indexes)) #"jmp" "nop")))]
        (if (not (first infinite?))
         infinite?
          (recur (rest indexes)))))))
      
  