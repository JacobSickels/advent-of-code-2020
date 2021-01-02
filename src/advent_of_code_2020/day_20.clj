(ns advent-of-code-2020.day-20
  (:require [advent-of-code-2020.core :as core]
            [clojure.set :as set]))

(def tile ["..##.#..#."
           "##..#....."
           "#...##..#."
           "####.#...#"
           "##.##.###."
           "##...#.###"
           ".#.#.#..##"
           "..#....#.."
           "###...#.#."
           "..###..###"])

(defn format-tiles [input]
  (map (fn [tile] [(first tile) (rest tile)])
    (remove #(= (first %) "") (partition-by empty? input))))

(defn get-edges [tile]
  (set (list (first tile)
             (apply str (map first tile))
             (apply str (map last tile))
             (last tile)
             (apply str (reverse (first tile)))
             (apply str (reverse (map first tile)))
             (apply str (reverse (map last tile)))
             (apply str (reverse (last tile))))))

(defn check-tile-against-rest [tile tiles]
  (let [but-self (remove #(= tile %) tiles)]
    (map (fn [[check-tile adjacent-tiles]] (list check-tile (first adjacent-tiles)))
      (remove (fn [[_ adjacent-tiles]] (empty? (second adjacent-tiles)))
        (map #(list (first tile) 
                (list (first %) 
                    (set/intersection (get-edges (second tile)) 
                                      (get-edges (second %))))) 
             but-self)))))

(defn day-20 []
  (let [data (format-tiles (core/read-file "resources/2020-20.txt"))]
    (filter #(= (count %) 2)
            (map #(check-tile-against-rest % data) data))))

(defn cleanup-tile-ids []
  (let [data (format-tiles (core/read-file "resources/2020-20.txt"))]
    (map (fn [l] (list (ffirst l) (map second l)))
         (map #(check-tile-against-rest % data) data))))

(defn make-sea-map []
  (let [tile-ids (cleanup-tile-ids)
        corners (filter #(= (count (second %)) 2) tile-ids)
        sides (filter #(= (count (second %)) 3) tile-ids)]
    (println (first corners))
    (remove #(empty? (set/intersection (set (second (first corners))) (set (second %)))) sides)))
          