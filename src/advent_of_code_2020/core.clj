(ns advent-of-code-2020.core
  (:require [clojure.math.combinatorics :as combo]))

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn xor [& args]
  (not (apply = args)))


(defn points-around [point]
  (remove (fn [p] (= point p))
          (apply combo/cartesian-product 
                 (map #(range (dec %) (+ 2 %)) point))))