(ns advent-of-code-2020.day-12)

(defn format-data [input]
  (map #(list (first %) (Integer/parseInt (apply str (rest %))))
       input))

(defn next-point [[d change] [x y] facing]
  (case d
    \N [[x (+ y change)] facing]
    \S [[x (- y change)] facing]
    \E [[(+ x change) y] facing]
    \W [[(- x change) y] facing]
    \L (let [new-facing (case facing
                          \N (get {0 \N 90 \W 180 \S 270 \E } change)
                          \E (get {0 \E 90 \N 180 \W 270 \S } change)
                          \S (get {0 \S 90 \E 180 \N 270 \W } change)
                          \W (get {0 \W 90 \S 180 \E 270 \N } change))]
         [[x y] new-facing])
         
    \R (let [new-facing (case facing
                          \N (get {0 \N 90 \E 180 \S 270 \W } change)
                          \E (get {0 \E 90 \S 180 \W 270 \N } change)
                          \S (get {0 \S 90 \W 180 \N 270 \E } change)
                          \W (get {0 \W 90 \N 180 \E 270 \S } change))]
         [[x y] new-facing])
    \F (case facing 
         \N [[x (+ y change)] facing]
         \E [[(+ x change) y] facing]
         \S [[x (- y change)] facing]
         \W [[(- x change) y] facing])))

(defn day-12 [input]
  (let [data (format-data input)]
    (loop [directions data
           point [0 0]
           facing \E
           acc []]
      (if (empty? directions)
        (apply + (map #(Math/abs %) (last acc)))
        (let [[next-point next-facing] (next-point (first directions) point facing)]
          (recur (rest directions) next-point next-facing (conj acc next-point)))))))


; Part 2 ~~~~~~~~~~~~

(defn rotate-point [[x y] [wX wY] direction]
  (case direction
    \L [[x y] [(- 0 wY) wX]]
    \R [[x y] [wY (- 0 wX)]]))

(defn rotate-point-times [[x y] [wX wY] direction times]
  (loop [amt times
         point [[x y] [wX wY]]]
    (if (zero? amt)
      point
      (recur (dec amt) (rotate-point (first point) (second point) direction)))))

(defn next-point-2 [[d change] [x y] [wX wY] facing]
  (case d
    \N [[x y] [wX (+ wY change)] facing]
    \S [[x y] [wX (- wY change)] facing]
    \E [[x y] [(+ wX change) wY] facing]
    \W [[x y] [(- wX change) wY] facing]
    \L (conj (rotate-point-times [x y] [wX wY] \L (/ change 90)) facing)
    \R (conj (rotate-point-times [x y] [wX wY] \R (/ change 90)) facing)
    \F [[(+ x (* change wX)) (+ y (* change wY))] [wX wY] facing]))

(defn day-12-2 [input]
  (let [data (format-data input)]
    (loop [directions data
           point [0 0]
           waypoint [10 1]
           facing \E
           acc []]
      (if (empty? directions)
        (apply + (map #(Math/abs %) (last acc)))
        (let [[next-point next-waypoint next-facing] (next-point-2 (first directions) point waypoint facing)]
          (recur (rest directions) next-point next-waypoint next-facing (conj acc next-point)))))))
    