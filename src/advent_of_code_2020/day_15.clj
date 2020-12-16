(ns advent-of-code-2020.day-15)

(defn next-number [last-num index number-lookups]
  (let [last-num-indexes (get number-lookups last-num)]
    (if (= 1 (count last-num-indexes))
      [0 (assoc number-lookups 0 (take 2 (concat [index] (get number-lookups 0))))]
      (let [next-num (reduce - (take 2 last-num-indexes))]
        [next-num (assoc number-lookups next-num (take 2 (concat [index] (get number-lookups next-num))))]))))

(defn do-sequence []
  (loop [index 7
         ;index 4
         ;number-lookups {0 [1]
         ;                3 [2]
         ;                6 [3]}
         number-lookups {13 [1]
                         16 [2]
                         0  [3]
                         12 [4]
                         15 [5]
                         1 [6]}
         recent-num 1]
    (let [[next-num lookups] (next-number recent-num index number-lookups)]
      (if (= index 30000000)
        next-num
        (recur (inc index) lookups next-num)))))
      