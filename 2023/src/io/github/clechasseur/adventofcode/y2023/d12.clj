(ns io.github.clechasseur.adventofcode.y2023.d12
  (:require [io.github.clechasseur.adventofcode.y2023.input.d12 :refer [input]]
            [io.github.clechasseur.adventofcode.util.string :refer [safe-subs]]
            [clojure.string :as str]))

(defn group-find
  [line group start cur last?]
  (and
    (not (str/includes? (safe-subs line start cur) "#"))
    (not (str/includes? (safe-subs line cur (+ cur group)) "."))
    (not= \# (get line (+ cur group)))
    (or (not last?) (not (str/includes? (safe-subs line (+ cur group)) "#")))))

(defn valid-ranges
  [line group start last? needed-space]
  (let [start-range (range start (- (count line) group -1 needed-space))
        valid (map (fn [cur]
                     (when (group-find line group start cur last?)
                       [cur (+ cur group)]))
                   start-range)]
    (filter some? valid)))

(def num-arrangements
  (memoize
    (fn
      ([row] (num-arrangements row 0))
      ([[line [group & groups]] start]
       (let [needed-space (+ (apply +' groups) (count groups))
             valid-ranges (valid-ranges line group start (empty? groups) needed-space)
             num-arrangements-if-more #(if (empty? groups) 1 (num-arrangements [line groups] %))
             totals-per-range (map (fn [[_ end]] (num-arrangements-if-more (inc end))) valid-ranges)]
         (apply +' totals-per-range))))))

(defn total-num-arrangements
  [rows]
  (->> rows
       (pmap num-arrangements)
       (apply +')))

(defn part-1
  []
  (total-num-arrangements input))

(defn part-2
  []
  (let [times-n (fn [n [line groups]] [(str/join "?" (repeat n line)) (flatten (repeat n groups))])
        input (map (partial times-n 5) input)]
    (total-num-arrangements input)))
