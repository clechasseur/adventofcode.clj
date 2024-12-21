(ns io.github.clechasseur.adventofcode.y2023.d04
  (:require [io.github.clechasseur.adventofcode.y2023.input.d04 :refer [input]]
            [clojure.set :refer [intersection]]
            [clojure.math :refer [pow]]))

;; An example of the data format for the input:
;; {:card 1 :winning-numbers #{23 42 66} :numbers #{11 7 42 12 67 13 23}}

(defn card
  [card-num]
  (first (filter #(= card-num (:card %)) input)))

(defn num-winners
  [{win-nums :winning-numbers nums :numbers}]
  (count (intersection win-nums nums)))

(defn part-1
  []
  (->> input
       (map #(int (pow 2 (dec (num-winners %)))))
       (apply +)))

(defn part-2
  []
  (let [copies (loop [copies {}
                      card-num (:card (last input))]
                 (if (zero? card-num)
                   copies
                   (recur
                     (let [num-winners (num-winners (card card-num))
                           won-cards (->> (range (inc card-num) (+ card-num num-winners 1))
                                          (map copies)
                                          (filter some?)
                                          (apply +)
                                          inc)]
                       (assoc copies card-num won-cards))
                     (dec card-num))))]
    (apply + (vals copies))))
