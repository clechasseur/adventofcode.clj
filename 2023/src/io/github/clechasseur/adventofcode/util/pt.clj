(ns io.github.clechasseur.adventofcode.util.pt
  (:require [clojure.math.combinatorics :as combo])
  (:refer-clojure :exclude [+ -]))

(def n+ clojure.core/+)
(def n- clojure.core/-)

(def + (partial mapv n+))
(def - (partial mapv n-))

(defn neighbours
  "Returns all points that are neighbours of the provided point. Works for any number of dimensions."
  [pt]
  (->> (repeat (count pt) [-1 0 1])
       (apply combo/cartesian-product)
       (filter #(not-every? (partial = 0) %))
       (map (partial + pt))))

(defn manhattan
  "Returns the Manhattan distance between two points. Works for any number of dimensions."
  [pt-a pt-b]
  {:pre [(= (count pt-a) (count pt-b))]}
  (apply n+ (map (comp abs n-) pt-a pt-b)))
