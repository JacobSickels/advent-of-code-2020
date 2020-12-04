(ns advent-of-code-2020.day-4
  (:require [advent-of-code-2020.core :as core]
            [clojure.string :as str]))

(defn make-passport [passport-str]
  (let [data (str/split passport-str #" ")
        key-vals (map #(str/split % #":") data)]
    (merge {:byr nil, :cid nil, :ecl nil, :eyr nil, :hcl nil, :hgt nil, :iyr nil, :pid nil}
           (into (sorted-map) (map (fn [[k v]] [(keyword k) v]) key-vals)))))

(defn validate-passport [passport]
  (let [nil-vals (filter (comp nil? val) passport)
        invalid-keys (remove #(= (first %) :cid) nil-vals)]
    (empty? invalid-keys)))

(defn validate-extra-fields [passport]
  (let [rules {:byr (fn [x] (if (nil? x)
                              false
                              (let [num (Integer/parseInt x)]
                                (and (= 4 (count x))
                                     (clojure.spec.alpha/int-in-range? 1920 2003 num)))))
               :iyr (fn [x] (if (nil? x)
                              false
                              (let [num (Integer/parseInt x)]
                               (and (= 4 (count x))
                                    (clojure.spec.alpha/int-in-range? 2010 2021 num)))))
               :eyr (fn [x] (if (nil? x)
                              false
                              (let [num (Integer/parseInt x)]
                                (and (= 4 (count x))
                                     (clojure.spec.alpha/int-in-range? 2020 2031 num)))))
               :hgt (fn [x] (if (nil? x)
                              false
                              (let [[coef measure] (rest (first (re-seq #"(\d+)(\S+)" x)))
                                    coefficient (Integer/parseInt coef)]
                                (cond
                                  (= measure "cm") (clojure.spec.alpha/int-in-range? 150 194 coefficient)
                                  (= measure "in") (clojure.spec.alpha/int-in-range? 59 77 coefficient)))))
               :hcl (fn [x] (if (nil? x)
                              false
                              (not (nil? (re-matches #"^#(?:[0-9a-fA-F]{3}){1,2}$" x)))))
               :ecl (fn [x] (some #(= x %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))
               :pid (fn [x] (if (nil? x)
                                false
                                (not (nil? (re-matches #"^\d{9}$" x)))))
               :cid (fn [x] (or (nil? x) true))}]
    (every? true? (map (fn [[k v]] ((k rules) v)) passport))))

(defn format-data []
  (let [data (core/read-file "resources/2020-4.txt")
        partition (partition-by #(= "" %) data)
        removed-empty (remove #(= (first %) "") partition)
        clean-strings (map #(str/join " " %) removed-empty)
        clean-data (map make-passport clean-strings)]
    clean-data))

(defn day-4 [] (count(filter validate-passport (format-data))))
(defn day-4-2 [] (count (filter validate-extra-fields (format-data))))    
    