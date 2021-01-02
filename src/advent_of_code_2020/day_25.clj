(ns advent-of-code-2020.day-25)


(defn transform [n subject]
  (rem (* n subject) 20201227))

(defn get-loop-size [encryption-key subject]
  (loop [n 1 size 0]
    (if (= n encryption-key)
      size
      (recur (transform n subject) (inc size)))))

(defn part-1 []
  (let [loop-size (get-loop-size 18499292 7)]
    (loop [n 0
           value 1]
      (if (= n loop-size)
        value
        (recur (inc n) (transform 8790390 value))))))
        