(ns io.github.clechasseur.adventofcode.y2023.d09
  (:require [io.github.clechasseur.adventofcode.y2023.input.d09 :refer [input]]))

(defn pairs
  [coll]
  (map vector coll (drop 1 coll)))

(defn unstack
  [stack]
  (loop [stack stack
         prev-last (last (peek stack))]
    (let [stack (pop stack)
          cur-last (+ (last (peek stack)) prev-last)]
      (if (= 1 (count stack))
        cur-last
        (recur stack cur-last)))))

(defn compute-next
  [history]
  (loop [stack (list history)]
    (if (every? zero? (peek stack))
      (unstack stack)
      (let [next-level (mapv (fn [[a b]] (- b a)) (pairs (peek stack)))]
        (recur (conj stack next-level))))))

(defn part-1
  []
  (apply + (map compute-next input)))

(defn part-2
  []
  (apply + (map compute-next (map reverse input))))
