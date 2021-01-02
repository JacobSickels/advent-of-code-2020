(ns advent-of-code-2020.day-24
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]))

(defn split-directions [directions]
  (loop [dir directions
         acc []]
    (if (empty? dir)
      acc
      (case (first dir)
        \e (recur (rest dir) (conj acc (str (first dir))))
        \w (recur (rest dir) (conj acc (str (first dir))))
        \s (recur (drop 2 dir) (conj acc (apply str (take 2 dir))))
        \n (recur (drop 2 dir) (conj acc (apply str (take 2 dir))))))))

(defn directions-to-tile [directions-str]
  (let [directions (split-directions directions-str)]
    (loop [dirs directions
           [posH posV] [0 0 0]]
      (if (empty? dirs)
        [posH posV]
        (case (first dirs)
          "e" (recur (rest dirs) [(inc posH) (dec posV)])
          "w" (recur (rest dirs) [(dec posH) (inc posV)])
          "ne" (recur (rest dirs) [(inc posH) posV])
          "nw" (recur (rest dirs) [posH (inc posV)])
          "se" (recur (rest dirs) [posH (dec posV)])
          "sw" (recur (rest dirs) [(dec posH) posV]))))))

(defn black-to-white []
  (let [flipped-tiles (map directions-to-tile (core/read-file "resources/2020-24.txt"))]
    (count (remove #(even? (val %)) (frequencies flipped-tiles)))))

(def test-data ["sesenwnenenewseeswwswswwnenewsewsw"
                "neeenesenwnwwswnenewnwwsewnenwseswesw"
                "seswneswswsenwwnwse"
                "nwnwneseeswswnenewneswwnewseswneseene"
                "swweswneswnenwsewnwneneseenw"
                "eesenwseswswnenwswnwnwsewwnwsene"
                "sewnenenenesenwsewnenwwwse"
                "wenwwweseeeweswwwnwwe"
                "wsweesenenewnwwnwsenewsenwwsesesenwne"
                "neeswseenwwswnwswswnw"
                "nenwswwsewswnenenewsenwsenwnesesenew"
                "enewnwewneswsewnwswenweswnenwsenwsw"
                "sweneswneswneneenwnewenewwneswswnese"
                "swwesenesewenwneswnwwneseswwne"
                "enesenwswwswneneswsenwnewswseenwsese"
                "wnwnesenesenenwwnenwsewesewsesesew"
                "nenewswnwewswnenesenwnesewesw"
                "eneswnwswnwsenenwnwnwwseeswneewsenese"
                "neswnwewnwnwseenwseesewsenwsweewe"
                "wseweeenwnesenwwwswnew"])

(defn get-tiles-around [[x y]]
  (let [p1 [(inc x) y]
        p2 [(inc x) (dec y)]
        p3 [x (dec y)]
        p4 [x (inc y)]
        p5 [(dec x) (inc y)]
        p6 [(dec x) y]]
    (conj [] p1 p2 p3 p4 p5 p6)))


(defn hex-is-adjacent? [[x y] pCheck]
  (let [p1 [(inc x) y]
        p2 [(inc x) (dec y)]
        p3 [x (dec y)]
        p4 [x (inc y)]
        p5 [(dec x) (inc y)]
        p6 [(dec x) y]]
    (contains? #{p1 p2 p3 p4 p5 p6} pCheck)))

(defn count-adjacent-tiles [tile tile-col]
  [tile (count (filter true? (map #(hex-is-adjacent? tile %) tile-col)))])

(defn next-state [black-tiles]
  (let [white-tiles (set/difference (set (apply concat (map get-tiles-around black-tiles))) (set black-tiles))
        flipped-to-black (reduce #(if (= (second %2) 2)
                                    (conj %1 (first %2))
                                    %1)
                                 []
                                 (map #(count-adjacent-tiles % black-tiles) white-tiles))
        ;flipped-to-black (map first (filter #(= (second %) 2) (map #(count-adjacent-tiles % black-tiles) white-tiles)))
        stay-black (reduce #(if (< 0 (second %2) 3)
                              (conj %1 (first %2))
                              %1)
                           []
                           (map #(count-adjacent-tiles % black-tiles) black-tiles))]
        ;stay-black (map first (filter #(< 0 (second %) 3) (map #(count-adjacent-tiles % black-tiles) black-tiles)))]
    (concat flipped-to-black stay-black)))


(defn hex-game-of-life []
  (let [data test-data
        ; data (core/read-file "resources/2020-24.txt")
        flipped-tiles (map directions-to-tile data)
        initial-black-tiles (set (map first (remove #(even? (val %)) (frequencies flipped-tiles))))]
    ; counting white tiles that are adjacent to black
    (loop [bt initial-black-tiles
           day 0]
      (println day)
      (if (= day 100)
        (count bt)
        (recur (next-state bt) (inc day))))))
    
  