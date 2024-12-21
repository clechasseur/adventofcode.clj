(ns io.github.clechasseur.adventofcode.y2023.d11
  (:require [io.github.clechasseur.adventofcode.y2023.input.d11 :refer [input]]
            [io.github.clechasseur.adventofcode.util.pt :as pt]
            [io.github.clechasseur.adventofcode.util.grid :as grid]
            [clojure.math.combinatorics :as combo]))

(defn empty-indices
  [universe]
  (->> (map-indexed
         (fn [y line]
           (when (every? (partial = \.) line)
             y))
         universe)
       (filter some?)))

(def empty-rows
  (memoize (fn [] (empty-indices input))))
(def empty-cols
  (memoize (fn [] (empty-indices (apply map vector input)))))

(defn translate-index
  [exp empty index]
  (->> (take-while (partial > index) empty)
       count
       (* (dec exp))
       (+ index)))

(defn translate-pt
  [exp pt]
  (mapv (partial translate-index exp) [(empty-cols) (empty-rows)] pt))

(defn stars
  [exp]
  (map (partial translate-pt exp) (grid/locate (partial = \#) input)))

(defn stars-manhattan-sum
  [exp]
  (->> (stars exp)
       (#(combo/combinations % 2))
       (map (partial apply pt/manhattan))
       (reduce +)))

(defn part-1
  []
  (stars-manhattan-sum 2))

(defn part-2
  []
  (stars-manhattan-sum 1000000))
