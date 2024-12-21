(ns io.github.clechasseur.adventofcode.y2023.d07
  (:require [io.github.clechasseur.adventofcode.y2023.input.d07 :refer [input]]
            [clojure.math :refer [pow]]
            [clojure.math.combinatorics :as combo]))

(def card-ranks
  [:A :K :Q :J :T :9 :8 :7 :6 :5 :4 :3 :2 :loljk])

(def hand-types
  [:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pairs :one-pair :high-card])

(defn rank
  [values value]
  (->> (.indexOf values value)
       (- (count values))))
(def card-rank (partial rank card-ranks))
(def hand-type-rank (partial rank hand-types))

(defn card-to-match
  [cards]
  (or (first (filter (partial not= :loljk) cards)) :loljk))

(defn matches?
  [expected card]
  (or (= expected card) (= :loljk card)))

(defn all-same?
  [cards]
  (let [expected (card-to-match cards)]
    (every? (partial matches? expected) cards)))

(defn hand-type
  [{cards :cards}]
  (let [same (fn [n cards] (->> (combo/combinations (map-indexed vector cards) n)
                                (filter #(all-same? (map second %)))
                                (map #(map first %))))
        same? (fn [n cards] (seq (same n cards)))
        full-house? (fn [cards]
                      (let [same3 (same 3 cards)
                            same2 (same 2 cards)]
                        (when (and (seq same3) (seq same2))
                          (->> (combo/cartesian-product same3 same2)
                               (map (partial apply concat))
                               (map distinct)
                               (map count)
                               (filter (partial = 5))
                               seq))))]
    (cond
      (same? 5 cards)              :five-of-a-kind
      (same? 4 cards)              :four-of-a-kind
      (full-house? cards)          :full-house
      (same? 3 cards)              :three-of-a-kind
      (= 2 (count (same 2 cards))) :two-pairs
      (same? 2 cards)              :one-pair
      :else                        :high-card)))

(defn sort-hands
  [hands]
  (let [hand-rank (fn [{cards :cards :as hand}]
                    (let [t-rank (hand-type-rank (hand-type hand))
                          c-ranks (map card-rank cards)]
                      (apply + (map-indexed #(* (pow 15 %1) %2) (concat (reverse c-ranks) [t-rank])))))]
    (sort-by hand-rank hands)))

(defn wildify
  [{cards :cards bid :bid}]
  {:cards (replace {:J :loljk} cards) :bid bid})

(defn winnings
  [hands]
  (->> (sort-hands hands)
       (map-indexed #(* (inc %1) (:bid %2)))
       (apply +)))

(defn part-1
  []
  (winnings input))

(defn part-2
  []
  (winnings (map wildify input)))
