(ns io.github.clechasseur.adventofcode.y2023.d06
  (:require [io.github.clechasseur.adventofcode.y2023.input.d06 :refer [input]]))

(defn distance-with-charge
  [charge-time total-time]
  (* charge-time (- total-time charge-time)))

(defn beats-record
  [charge-time {total-time :time prev-record :distance}]
  (> (distance-with-charge charge-time total-time) prev-record))

(defn num-records
  [{total-time :time :as race}]
  (let [half-point (inc (quot total-time 2))
        smallest-charge-time (loop [charge-time (quot half-point 2)
                                    this-jump (quot charge-time 2)]
                               (let [this-valid (beats-record charge-time race)
                                     prev-valid (and (> charge-time 1) (beats-record (dec charge-time) race))
                                     next-jump (max (quot this-jump 2) 1)]
                                 (cond
                                   (= [this-valid prev-valid] [true false]) charge-time
                                   (= [this-valid prev-valid] [true true])  (recur (- charge-time next-jump) next-jump)
                                   :else                                    (recur (+ charge-time next-jump) next-jump))))

        ;; Original implementation (brute-force):
        ;; smallest-charge-time (loop [charge-time 1]
        ;;                        (if (> (distance-with-charge charge-time total-time) prev-record)
        ;;                          charge-time
        ;;                          (recur (inc charge-time))))
        ]
    (-> (inc total-time)
        (quot 2)
        (- smallest-charge-time)
        (* 2)
        (+ (mod (inc total-time) 2)))))

(defn part-1
  []
  (apply * (map num-records (:races input))))

(defn part-2
  []
  (num-records (:big-race input)))
