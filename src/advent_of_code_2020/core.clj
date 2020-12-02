(ns advent-of-code-2020.core)

(defn read-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce conj [] (line-seq reader))))

(defn xor [& args]
  (and (some true? (vec args)) (not (every? true? (vec args)))))