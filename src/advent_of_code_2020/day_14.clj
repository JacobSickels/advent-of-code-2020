(ns advent-of-code-2020.day-14
  (:require [clojure.string :as str]
            [advent-of-code-2020.core :as core]))

(defn pad-binary-string [number]
  (clojure.pprint/cl-format nil "~36,'0d" (Integer/toBinaryString number)))

; binary string needs to be padded here
(defn apply-bitmask [bitmask binary-string]
  (let [ignore-bits (filter #(= (second %) \X) (map-indexed (fn [idx x] [idx x]) bitmask))
        binary-string-bits (map #(get binary-string %) (map first ignore-bits))]
    (apply format (str/replace bitmask #"X" "%s") binary-string-bits)))

(defn program-chunk-memory [mask memory-assignments]
  (map #(list (first %) (apply-bitmask (second mask) (second %))) memory-assignments))


(defn day-14 []
      (let [data (core/read-file "resources/2020-14.txt")
            splitted (map #(str/split % #" = ") data)
            partitioned (map (fn [x] (partition 2 x))
                             (map flatten (partition 2 (partition-by #(= (first %) "mask") splitted))))
            clean-memory (map #(map (fn [[memory val]]
                                      (list memory (pad-binary-string (Integer/parseInt val))))
                                    (rest %))
                              partitioned)
            with-masks (map #(list %1 %2) (map first partitioned) clean-memory)
            applied-masks (map #(program-chunk-memory (first %) (second %)) with-masks)]
        (into {} (map (fn [[k v]] [k (Long/parseLong v 2)])
                      (partition 2 (flatten applied-masks))))))

(defn apply-floating-bitmask [bitmask binary-memory-string]
  (let [amt-xs (get (frequencies bitmask) \X)
        binary-replacements (map #(Integer/toBinaryString %) (range 0 (Math/pow 2 amt-xs)))
        max-binary (apply max (map count binary-replacements))
        clean-binary-replacements (map #(str/split (clojure.pprint/cl-format nil (str "~" max-binary ",'0d") %) #"") binary-replacements)]
    (map #(apply format (str/replace bitmask #"X" "%s") %) clean-binary-replacements)))
; Almost there here, starting making the applied masks for all the different bits, haven't applied the original binary memory string to mask beforehand
    

(defn get-all-memory-changes [mask memory]
  ())
(defn program-chunk-memory-mask [mask memory-assignments]
  ())

(defn day-14-2 []
  (let [data (core/read-file "resources/2020-14.txt")
        splitted (map #(str/split % #" = ") data)
        partitioned (map (fn [x] (partition 2 x))
                         (map flatten (partition 2 (partition-by #(= (first %) "mask") splitted))))
        clean-memory (map #(map (fn [[memory val]]
                                  (list memory (pad-binary-string (Integer/parseInt val))))
                                (rest %))
                          partitioned)
        with-masks (map #(list %1 %2) (map first partitioned) clean-memory)
        applied-masks (map #(program-chunk-memory (first %) (second %)) with-masks)]
    with-masks))

(defn apply-memory-mask [mask memory-address value]
  (let [floating-mask (apply str (map #(cond 
                                         (= %1 \X) \X
                                         (= %1 \1) \1
                                         (and (= %1 \0) (= %2 \1)) \1
                                         (and (= %1 \0) (= %2 \0)) \0)
                                      mask memory-address))
        amt-xs (get (frequencies floating-mask) \X)
        binary-replacements (map #(Integer/toBinaryString %) (range 0 (Math/pow 2 amt-xs)))
        max-binary (apply max (map count binary-replacements))
        clean-binary-replacements (map #(str/split (clojure.pprint/cl-format nil (str "~" max-binary ",'0d") %) #"") binary-replacements)]
    (map
      #(list % value)
      (map #(apply format (str/replace floating-mask #"X" "%s") %) clean-binary-replacements))))

(defn day-14-2-2 []
  (let [data (core/read-file "resources/2020-14.txt")
        splitted (map #(str/split % #" = ") data)
        partitioned (map (fn [x] (partition 2 x))
                         (map flatten (partition 2 (partition-by #(= (first %) "mask") splitted))))
        clean-maps (map #(hash-map :mask (second (first %)) 
                                   :memory (map (fn [[address value]] 
                                                  [(pad-binary-string (Integer/parseInt (re-find #"\d+" address))) 
                                                   (pad-binary-string (Integer/parseInt value))]) (rest %)))
                        partitioned)]
    (reduce + 
            (vals (into {} (map (fn [[k v]] [k (Long/parseLong v 2)])
                                (partition 2 
                                           (flatten (map #(map (fn [[address value]] 
                                                                 (apply-memory-mask (:mask %) address value)) 
                                                               (:memory %)) 
                                                         clean-maps)))))))))
    
       