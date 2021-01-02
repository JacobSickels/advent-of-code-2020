(ns advent-of-code-2020.day-13
  (:require [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(def test-data
  ["939"
   "7,13,x,x,59,x,31,19"])

(defn day-13 []
  (let [data (core/read-file "resources/2020-13.txt")
        earliest (Integer/parseInt (first data))
        busses (map (fn [x] (Integer/parseInt x)) (remove #(= % "x") (str/split (second data) #",")))]
    (sort-by last (map #(let [bus-id %
                              closest-departure (* (int (Math/ceil (/ earliest %))) %)
                              minutes-waiting (- closest-departure earliest)]
                          (list bus-id closest-departure minutes-waiting))
                       busses))))

(defn starts-series [[series-id next-bus-id offset]]
  (println [series-id next-bus-id offset])
  (loop [pos 0
         acc []]
    (if (= (count acc) 2)
      {:bus-id series-id :next-bus-id next-bus-id :offset offset :start-index (first acc) :series-count (- (second acc) (first acc))}
      (if (zero? (rem (+ offset (* series-id pos)) next-bus-id))
        (recur (inc pos) (conj acc (* series-id pos)))
        (recur (inc pos) acc)))))

(defn find-series-length [series-id next-bus-id offset]
  (loop [pos 0
         acc []]
    (if (= (count acc) 2)
      {:start-id (first acc) :series-length (apply - (reverse acc))}
      (if (zero? (rem (+ offset (* series-id pos)) next-bus-id))
        (recur (inc pos) (conj acc (* series-id pos)))
        (recur (inc pos) acc)))))


(defn is-position-valid? [pos series]
  (every? true? (map #(zero? (rem (+ pos (:offset %)) (:next-bus-id %))) series)))


; 7-13 series length 91 start 77
; 7-13-59 series length 5369 start index 350
; 7-13-59-31 series length 166439 index 7903

(defn get-grouping [[starting-pos series-length] check-id offset]
  (loop [position starting-pos
         acc []]
    (if (= (count acc) 2)
      [(first acc) (apply - (reverse acc))]
      (if (zero? (rem (+ position series-length offset) check-id))
        (recur (+ position series-length) (conj acc (+ position series-length)))
        (recur (+ position series-length) acc)))))

(find-series-length 7 13 1)

(defn day-13-2 []
  (let [data (core/read-file "resources/2020-13.txt")
        ids (remove
              #(= (first %) 1)
              (map-indexed
                (fn [idx n] [n idx])
                (map (fn [x] (if (= "x" x) 1 (Integer/parseInt x)))
                     (str/split (second data) #","))))
        initial-series (find-series-length 17 37 11)
        initial-starting-pos (:start-id initial-series)
        initial-series-length (:series-length initial-series)]
    (loop [buses (rest (rest ids))
           starting-pos initial-starting-pos
           series-length initial-series-length]
      
      (if (empty? buses)
        starting-pos
        (let [[check-bus-id bus-offset] (first buses)
              [next-starting-pos next-series-length] (get-grouping [starting-pos series-length] check-bus-id bus-offset)]
          (recur (rest buses) next-starting-pos next-series-length))))))
        
      
        

    
    
      
           
    
                    
      