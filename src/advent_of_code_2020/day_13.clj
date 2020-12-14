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

(defn number-seq
  ([start] (number-seq 0 start))
  ([n start] (lazy-seq (cons n (number-seq (+ n start) start)))))

; [index start-index series-length]
; [7 11 13]  
; [13 27 59]

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn starts-series [[series-id next-bus-id offset]]
  (println [series-id next-bus-id offset])
  (loop [pos 0
         acc []]
    (if (= (count acc) 2)
      {:bus-id series-id :next-bus-id next-bus-id :offset offset :start-index (first acc) :series-count (- (second acc) (first acc))}
      (if (zero? (rem (+ offset (* series-id pos)) next-bus-id))
        (recur (inc pos) (conj acc (* series-id pos)))
        (recur (inc pos) acc)))))

(defn day-13-2 []
  (let [data test-data
        busses (map (fn [x] (if (= "x" x) 1 (Integer/parseInt x))) (str/split (second data) #","))]
    (map starts-series
         (map (fn [[[s o] [n p]]] [s n (- p o)])
              (partition 2 1
                         (remove #(= (first %) 1)
                                 (map-indexed (fn [idx i] [i idx]) busses)))))))


;(loop [all-jumps start-jumps
;       position (second (first all-jumps))
;       [bus-id _ jump] (first all-jumps)]
;  (if (empty? all-jumps)
;    position
;    (if (zero? (rem position bus-id))
;      (recur (rest all-jumps) (inc position) (first (rest all-jumps)))
;      (recur))))

(defn find-series-length [[series-id next-bus-id offset]]
  (loop [pos 0
         acc []]
    (if (= (count acc) 2)
      (apply - (reverse acc))
      (if (zero? (rem (+ offset (* series-id pos)) next-bus-id))
        (recur (inc pos) (conj acc (* series-id pos)))
        (recur (inc pos) acc)))))


(defn is-position-valid? [pos series]
  (every? true? (map #(zero? (rem (+ pos (:offset %)) (:next-bus-id %))) series)))


; 7-13 series length 91 start 77
; 7-13-59 series length 5369 start index 2534
; 7-13-59-31 series length 166439 index 7903

(defn get-grouping [[starting-pos series-length] check-id offset]
  (loop [position starting-pos
         acc []]
    (if (= (count acc) 2)
      [(first acc) (apply - (reverse acc))]
      (if (zero? (rem (+ position series-length offset) check-id))
        (recur (+ position series-length) (conj acc (+ position series-length)))
        (recur (+ position series-length) acc)))))
          
(defn get-final []
  (let [buses (map (fn [x] (if (= "x" x) 1 (Integer/parseInt x))) (str/split (second test-data) #","))
        bus-offsets (map (fn [[[s o] [n p]]] [n (- p o)])
                         (partition 2 1
                                    (remove #(= (first %) 1)
                                            (map-indexed (fn [idx i] [i idx]) buses))))]
    (loop [b-o (concat [[(first buses) 0]] bus-offsets)
           acc []]
      (println b-o)
      (if (= (count b-o) 1)
       acc
       (let [[check-id offset] (second b-o)
             grouping (get-grouping (reverse (first b-o)) check-id offset)]
         (recur (concat [grouping] (drop 2 b-o)) (conj acc grouping)))))))
        

    
    
      
           
    
                    
      