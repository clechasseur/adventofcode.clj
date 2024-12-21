(ns io.github.clechasseur.adventofcode.y2023.d03
  (:require [io.github.clechasseur.adventofcode.y2023.input.d03 :refer [input]]
            [io.github.clechasseur.adventofcode.util.pt :as pt]
            [io.github.clechasseur.adventofcode.util.string :as str]))

(defn nums-around
  [pt]
  (->> (pt/neighbours pt)
       (map
         (fn [[x y]]
           (when-let [row (get input y)]
             (when (and (>= x 0) (< x (count row)) (Character/isDigit ^char (get row x)))
               (let [start (loop [x x]
                            (if (or (< x 0) (not (Character/isDigit ^char (get row x))))
                              (inc x)
                              (recur (dec x))))
                     end (loop [x (inc start)]
                           (if (or (>= x (count row)) (not (Character/isDigit ^char (get row x))))
                             x
                             (recur (inc x))))]
                 (Integer/parseInt (subs row start end)))))))
       (filter some?)
       distinct))

(defn part-1
  []
  (->> (range (count input))
       (map
         (fn [row-idx]
           (let [row (get input row-idx)
                 m (re-matcher #"\d+" row)]
             (loop [nums []]
               (let [found (.find m)]
                 (if-not found
                   nums
                   (let [num-start (.start m)
                         num-end (.end m)
                         above-num (if-let [prev-row (get input (dec row-idx))]
                                     (str/safe-subs prev-row (dec num-start) (inc num-end))
                                     "")
                         below-num (if-let [next-row (get input (inc row-idx))]
                                     (str/safe-subs next-row (dec num-start) (inc num-end))
                                     "")
                         before-num (if-let [before (get row (dec num-start))] (str before) "")
                         after-num (if-let [after (get row num-end)] (str after) "")
                         all (str above-num below-num before-num after-num)]
                     (if (re-find #"[^\d.]" all)
                       (recur (conj nums (Integer/parseInt (re-groups m))))
                       (recur nums)))))))))
       flatten
       (apply +)))

(defn part-2
  []
  (->> (range (count input))
       (map
         (fn [row-idx]
           (let [row (get input row-idx)
                 m (re-matcher #"\*" row)]
             (loop [gears []]
               (let [found (.find m)]
                 (if-not found
                   gears
                   (let [col-idx (.start m)
                         around (nums-around [col-idx row-idx])]
                     (if (= (count around) 2)
                       (recur (conj gears (apply * around)))
                       (recur gears)))))))))
       flatten
       (apply +)))
