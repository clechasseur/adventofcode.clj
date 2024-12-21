(ns io.github.clechasseur.adventofcode.y2023.d02
  (:require [io.github.clechasseur.adventofcode.y2023.input.d02 :refer [input]]))

(defn parse-game
  [game]
  (let [[_ id reveals] (re-find #"^Game (\d+): (.*)$" game)]
    (->> reveals
         (re-seq #"[^; ][^;]*")
         (map #(re-seq #"(\d+) (blue|red|green)" %))
         (map #(map (fn [[_ count color]] (vector color (Integer/parseInt count))) %))
         (map #(into (sorted-map) %))
         (list (Integer/parseInt id)))))

(def games
  (map parse-game input))

(def part-1-targets
  {"red" 12 "green" 13 "blue" 14})

(defn part-1
  []
  (->> games
       (filter
         (fn [[_ reveals]]
           (->> reveals
                (map #(map (fn [[color count]] (<= count (part-1-targets color))) %))
                flatten
                (reduce #(and %1 %2)))))
       (map first)
       (apply +)))

(defn part-2
  []
  (->> games
       (map
         (fn [[_ reveals]]
           (->> reveals
                (apply (partial merge-with max))
                vals
                (apply *))))
       (apply +)))
