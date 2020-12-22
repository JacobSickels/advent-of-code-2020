(ns advent-of-code-2020.day-22)

(def player-1 [26 14 6 34 37 9 17 39 4 5 1 8 49 16 18 47 20 31 23 19 35 41 28 15 44])
(def player-2 [7 2 10 25 29 46 40 45 11 50 42 24 38 13 36 22 33 3 43 21 48 30 32 12 27])

(defn day-22 []
  (loop [deck-1 player-1
         deck-2 player-2]
    (if (or (empty? deck-1) (empty? deck-2))
      (reduce + (map #(* %1 %2) deck-2 (reverse (range 1 51))))
      (if (> (first deck-1) (first deck-2))
        (recur (conj (vec (rest deck-1)) (first deck-1) (first deck-2)) (rest deck-2))
        (recur (rest deck-1) (conj (vec (rest deck-2)) (first deck-2) (first deck-1)))))))

(defn recursive-combat [deck-1 deck-2]
  (loop [previous-games #{}
         player-1 deck-1
         player-2 deck-2]
    (cond
      (empty? player-1) [2 player-1 player-2]
      (empty? player-2) [1 player-1 player-2]

      :else
      (if (contains? previous-games [player-1 player-2])
        [1 player-1 player-2]
        (let [player-1-top (first player-1)
              player-2-top (first player-2)]
          (cond
            (and
              (> (count player-1) player-1-top)
              (> (count player-2) player-2-top))
            (let [recursive-winner (recursive-combat (take player-1-top (rest player-1))
                                                     (take player-2-top (rest player-2)))]
              (if (= 1 (first recursive-winner))
                (recur (conj previous-games [player-1 player-2])
                       (conj (vec (rest player-1)) player-1-top player-2-top)
                       (rest player-2))
                (recur (conj previous-games [player-1 player-2])
                       (rest player-1)
                       (conj (vec (rest player-2)) player-2-top player-1-top))))

            :else
            (if (> player-1-top player-2-top)
              (recur (conj previous-games [player-1 player-2])
                     (conj (vec (rest player-1)) player-1-top player-2-top)
                     (rest player-2))
              (recur (conj previous-games [player-1 player-2])
                     (rest player-1)
                     (conj (vec (rest player-2)) player-2-top player-1-top)))))))))

(defn day-22-2 []
  (let [[winner deck-1 deck-2] (recursive-combat player-1 player-2)]
    (if (= winner 1)
      (reduce + (map #(* %1 %2) deck-1 (reverse (range 1 51))))
      (reduce + (map #(* %1 %2) deck-2 (reverse (range 1 51)))))))
      
              