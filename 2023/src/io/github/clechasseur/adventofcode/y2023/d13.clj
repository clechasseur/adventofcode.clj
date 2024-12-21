(ns io.github.clechasseur.adventofcode.y2023.d13
  (:require [io.github.clechasseur.adventofcode.y2023.input.d13 :refer [input]]
            [io.github.clechasseur.adventofcode.util.string :refer [safe-subs]]
            [io.github.clechasseur.adventofcode.util.grid :as grid]
            [clojure.math.combinatorics :as combo]))

(defn transpose
  [pattern]
  (mapv (partial apply str) (grid/transpose pattern)))

(defn reflect-h
  [pattern flip-row]
  (let [below-flip (- (count pattern) flip-row 1)
        above-flip (- (count pattern) below-flip)
        num-flipped-rows (min above-flip below-flip)]
    (->> (range (count pattern))
         (mapv (fn [row]
                 (if (< flip-row row (+ flip-row num-flipped-rows 1))
                   (get pattern (inc (- flip-row (- row flip-row))))
                   (get pattern row)))))))

(defn find-flip-rows
  [pattern]
  (->> (range (dec (count pattern)))
       (map (fn [flip-row] (when (= (reflect-h pattern flip-row) pattern) flip-row)))
       (filter some?)))

(defn find-flip-cols
  [pattern]
  (find-flip-rows (transpose pattern)))

(defn single-flips
  [& flips]
  {:pre (every? #(<= (count %) 1) flips)}
  (mapv first flips))

(defn find-flips
  ([pattern] (find-flips pattern []))
  ([pattern [not-this-flip-row not-this-flip-col]]
   (let [filter-flip (fn [not-this-flip flip] (not= flip not-this-flip))
         flip-rows (filter (partial filter-flip not-this-flip-row) (find-flip-rows pattern))
         flip-cols (filter (partial filter-flip not-this-flip-col) (find-flip-cols pattern))]
     (single-flips flip-rows flip-cols))))

(defn summary
  [[flip-row flip-col]]
  (+ (inc (or flip-col -1)) (* 100 (inc (or flip-row -1)))))

(defn pattern-summary
  [pattern]
  (summary (find-flips pattern)))

(def inverse
  {\# \. \. \#})

(defn smudges
  [pattern]
  (->> (combo/cartesian-product (range (count (first pattern))) (range (count pattern)))
       (map (fn [[x' y']]
              (->> pattern
                   (map-indexed (fn [y row]
                                  (if (= y y')
                                    (str (safe-subs row 0 x') (inverse (get row x')) (safe-subs row (inc x')))
                                    row)))
                   (into []))))))

(defn smudged-pattern-summary
  [pattern]
  (let [normal-flips (find-flips pattern)]
    (->> (smudges pattern)
         (map #(find-flips % normal-flips))
         (map summary)
         distinct
         (apply +))))

(defn part-1
  []
  (apply + (map pattern-summary input)))

(defn part-2
  []
  (apply + (map smudged-pattern-summary input)))
