(ns advent-of-code-2020.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2020.core :as core]))

(def test-data ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
                "trh fvjkl sbzzf mxmxvkd (contains dairy)"
                "sqjhc fvjkl (contains soy)"
                "sqjhc mxmxvkd sbzzf (contains fish)"])

(defn parse-data [input]
  (map (fn [[ingr alrgy]] [(str/split (first ingr) #" ") alrgy])
       (map (fn [i]
              (map #(str/split (str/trim %) #", ")
                   (str/split (str/replace i #"contains " "") #"\(|\)")))
            input)))

(defn get-alergens [input]
  (set (flatten (map second input))))

(defn ingrs-have-alergen [input alergen]
  (apply set/intersection
         (map (fn [l] (set (first l)))
              (filter #(contains? (set (second %)) alergen) input))))

(defn ingredients->alergens [input]
  (let [alergens (get-alergens input)
        alergen->ingredients (map #(list % (ingrs-have-alergen input %)) alergens)
        init-found (filter #(= (count (second %)) 1) alergen->ingredients)]
    (loop [found-pairings init-found]
      (let [found-ingrs (apply set/union (map second found-pairings))
            checking (remove #(contains? (set found-pairings) %) alergen->ingredients)
            removed-already-found (map #(list (first %) (set/difference (second %) found-ingrs)) checking)
            empty-ingredients? (every? empty? (map second removed-already-found))]

        (if empty-ingredients?
          found-pairings
          (recur (concat found-pairings (filter #(= (count (second %)) 1) removed-already-found))))))))

(defn non-alergen-ingredients [input]
  (let [ingredients-with-alergens (ingredients->alergens input)
        found-ingredients (map second ingredients-with-alergens)]
    (set/difference (apply set/union (map set (map first input)))
                    (apply set/union found-ingredients))))

(defn lists-contain-ingredient [input ingredient]
  (filter #(contains? (set %) ingredient) (map first input)))

(defn day-21 []
  (let [data (parse-data (core/read-file "resources/2020-21.txt"))
        non-alergens (non-alergen-ingredients data)]
    (reduce + (map #(count (lists-contain-ingredient data %)) non-alergens))))

(defn day-21-2 []
  (let [data (parse-data (core/read-file "resources/2020-21.txt"))
        ingredients-with-alergens (ingredients->alergens data)]
    (str/join ","(map #(first (second %)) (sort-by first ingredients-with-alergens)))))
