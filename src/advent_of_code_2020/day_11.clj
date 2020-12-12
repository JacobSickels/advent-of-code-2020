(ns advent-of-code-2020.day-11
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]))


(def test-data ["L.LL.LL.LL"
                "LLLLLLL.LL"
                "L.L.L..L.."
                "LLLL.LL.LL"
                "L.LL.LL.LL"
                "L.LLLLL.LL"
                "..L.L....."
                "LLLLLLLLLL"
                "L.LLLLLL.L"
                "L.LLLLL.LL"])

(defn get-freq-values [freq]
  (let [hash-freq (or (get freq \#) 0)
        l-freq (or (get freq \L) 0)
        dot-freq (or (get freq \.) 0)]
    {\# hash-freq \L l-freq \. dot-freq}))

(defn next-cell-state [[x y] col width height]
  (let [points-around [[(dec x) (dec y)]
                       [(dec x) y]
                       [(dec x) (inc y)]
                       [x (dec y)]
                       [x (inc y)]
                       [(inc x) (dec y)]
                       [(inc x) y]
                       [(inc x) (inc y)]]
        in-bounds (remove (fn [[x y]]
                            (or (>= x height)
                                (< x 0)
                                (>= y width)
                                (< y 0)))
                          points-around)
        frequency (get-freq-values (frequencies (map (fn [[x y]] (nth (nth col x) y)) in-bounds)))]
    (case (nth (nth col x) y)
      \L (if (zero? (get frequency \#)) [\# 1] [\L 0])
      \# (if (>= (get frequency \#) 4) [\L 1] [\# 0])
      [\. 0])))

(defn next-state [col]
  (let [width (count (first col))
        height (count col)
        width-rng (range 0 width)
        height-rng (range 0 height)
        points (partition 2 (flatten (map #(map (fn [x] [% x]) width-rng) height-rng)))
        new-arrangement (map first (map #(next-cell-state % col width height) points))
        changed-seats (let [new-arr-freq (get-freq-values (frequencies new-arrangement))
                            old-arr-freq (get-freq-values (frequencies (flatten col)))]
                        [old-arr-freq new-arr-freq])]
    [(partition width new-arrangement) changed-seats]))

(defn day-11 [input]
  (loop [data (map seq input)]
    (let [[new-state [old-freq new-freq]] (next-state data)]
      (if (empty? (set/difference (set old-freq) (set new-freq)))
        old-freq
        (recur new-state)))))

; Part 2
(def test-data-ray [".......#."
                    "...#....."
                    ".#......."
                    "........."
                    "..#L....#"
                    "....#...."
                    "........."
                    "#........"
                    "...#....."])

(def test-data-ray-2 ["............."
                      ".L.L.#.#.#.#."
                      "............."])

(def test-data-ray-3 [".##.##."
                      "#.#.#.#"
                      "##...##"
                      "...L..."
                      "##...##"
                      "#.#.#.#"
                      ".##.##."])


(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn find-points-on-ray [[x y] [changeX changeY] width height]
  (loop [[pX pY] [x y]
         acc []]
      (let [[xNP yNP] [(+ pX changeX) (+ pY changeY)]]
        (if (or (= xNP width)
                (< xNP 0)
                (< yNP 0)
                (= yNP height))
          acc
          (recur [xNP yNP] (conj acc [xNP yNP]))))))

(defn find-seat-from-ray [[x y] [changeX changeY] col width height]
  (let [ray-points (find-points-on-ray [x y] [changeX changeY] width height)]
    (loop [points ray-points]
      (if (empty? points)
        [\. false]
        (let [[checkX checkY] (first points)]
          (case (nth (nth col checkY) checkX)
            \L[\L false]
            \# [\# true]
            \. (recur (rest points))))))))

(defn find-all-ray-seats [point col]
  (let [changes (partition 2 (flatten (map #(map (fn [x] [% x])
                                                 (range -1 2))
                                           (range -1 2))))
        clean-changes (remove (fn [[x y]] (and (= x 0) (= y 0))) changes)
        width (count (first col))
        height (count col)]
    (map #(find-seat-from-ray point % col width height) clean-changes)))
    
(defn next-cell-state-2 [point col]
  (let [self (nth (nth col (second point)) (first point))
        see-seats (get-freq-values (frequencies (map first (find-all-ray-seats point col))))]
    (case self 
      \# (if (>= (get see-seats \#) 5) \L \#)
      \L (if (zero? (get see-seats \#)) \# \L)
      \.)))
        

(defn next-state-2 [col]
  (let [width (count (first col))
        height (count col)
        width-rng (range 0 width)
        height-rng (range 0 height)
        points (partition 2 (flatten (map #(map (fn [x] [x %]) width-rng) height-rng)))
        new-arrangement (map #(next-cell-state-2 % col) points)
        changed-seats (let [new-arr-freq (get-freq-values (frequencies new-arrangement))
                            old-arr-freq (get-freq-values (frequencies (flatten col)))]
                        [old-arr-freq new-arr-freq])]
    [(partition width new-arrangement) changed-seats]))

(defn day-11-2 [input]
  (loop [data input]
    (let [[new-state [old-freq new-freq]] (next-state-2 data)]
      (if (empty? (set/difference (set old-freq) (set new-freq)))
        old-freq
        (recur new-state)))))
    
    
  
  
      

        
      
