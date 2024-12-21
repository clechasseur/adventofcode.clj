(ns io.github.clechasseur.adventofcode.y2023.d05
  (:require [io.github.clechasseur.adventofcode.y2023.input.d05 :refer [input]]))

;; An example of the data format for the input:
;; {:seeds [1 2 3 ...]
;;  :maps [{:name :seed-to-soil
;;          :ranges [{:dest-range-start 42 :source-range-start 23 :range-size 11}
;;                   ...]}
;;         {:name :soil-to-fertilizer
;;          :ranges [...]}
;;         ...
;;         {:name :humidity-to-location
;;          :ranges [...]}]}

;; For part 1:

(defn source-to-dest
  [ranges source]
  (->> ranges
       (map
         (fn [{dest-range-start :dest-range-start source-range-start :source-range-start range-size :range-size}]
           (when (<= source-range-start source (+ source-range-start range-size -1))
             (+ dest-range-start (- source source-range-start)))))
       (filter some?)
       first
       (#(or % source))))

(def maps
  (let [{maps :maps} input]
    (map
      (fn [{name :name ranges :ranges}]
        {:name name :fn #(source-to-dest ranges %)})
      maps)))

(defn seeds-to-location
  [seeds]
  (reduce
    (fn [sources {map-fn :fn}]
      (map map-fn sources))
    seeds
    maps))

;; For part 2:

(defn seed-ranges
  [seeds]
  (loop [result []
         [start length & rest :as seeds] seeds]
    (if (empty? seeds)
      result
      (recur
        (conj result [start (+ start length -1)])
        rest))))

(defn source-ranges-to-dest-ranges
  [source-ranges {map-ranges :ranges}]
  (loop [[range & rest :as source-ranges] source-ranges
         dest-ranges []]
    (if (empty? source-ranges)
      dest-ranges
      (recur
        rest
        (let [[dest-ranges remaining-ranges] (reduce
                                               (fn [[dest-ranges ranges]
                                                    {dest-range-start :dest-range-start source-range-start :source-range-start range-size :range-size}]
                                                 (->> (mapv
                                                        (fn [[range-first range-last :as range]]
                                                          (if (not-any? #(<= source-range-start % (+ source-range-start range-size -1)) range)
                                                            [nil [range]]
                                                            (let [source-range-first source-range-start
                                                                  source-range-last (+ source-range-start range-size -1)
                                                                  cut-first (max range-first source-range-first)
                                                                  cut-last (min range-last source-range-last)
                                                                  cut (when (<= cut-first cut-last)
                                                                        (->> [cut-first cut-last]
                                                                             (mapv #(+ dest-range-start (- % source-range-first)))))
                                                                  left-before (when (> cut-first range-first)
                                                                                [range-first (dec cut-first)])
                                                                  left-after (when (< cut-last range-last)
                                                                               [(inc cut-last) range-last])
                                                                  left (filterv some? [left-before left-after])]
                                                              [cut left])))
                                                        ranges)
                                                      (reduce
                                                        (fn [[dest-ranges remaining-ranges] [cut left]]
                                                          (let [dest-ranges (if (some? cut)
                                                                              (conj dest-ranges cut)
                                                                              dest-ranges)]
                                                            [dest-ranges (apply conj remaining-ranges left)]))
                                                        [dest-ranges []])))
                                               [dest-ranges [range]]
                                               map-ranges)]
          (apply conj dest-ranges remaining-ranges))))))

(defn seed-ranges-to-location-ranges
  [seed-ranges]
  (reduce source-ranges-to-dest-ranges seed-ranges (:maps input)))

;; -------------------------------------------------------------------

(defn part-1
  []
  (apply min (seeds-to-location (:seeds input))))

(defn part-2
  []
  (->> (:seeds input)
       seed-ranges
       seed-ranges-to-location-ranges
       (map first)
       (apply min)))
