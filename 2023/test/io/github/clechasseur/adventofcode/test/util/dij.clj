(ns ^:util io.github.clechasseur.adventofcode.test.util.dij
  (:require [clojure.test :refer :all]
            [io.github.clechasseur.adventofcode.util.pt :as pt]
            [io.github.clechasseur.adventofcode.util.grid :as grid]
            [io.github.clechasseur.adventofcode.util.dij :as dij]))

(def test-terrain
  ["123#298#3#"
   "1#0840137#"
   "###1112319"
   "###E9712#3"
   "9014551312"
   "923###56#1"
   "919#11119S"
   "8168#321#1"
   "#22#######"
   "3333333##!"])
(def start (grid/locate-one (partial = \S) test-terrain))
(def end (grid/locate-one (partial = \E) test-terrain))
(def stuck (grid/locate-one (partial = \!) test-terrain))
(def terrain-size (* (count test-terrain) (count (first test-terrain))))
(def num-obstacles (count (grid/locate (partial = \#) test-terrain)))
(defn neighbours
  [pt]
  (->> [[0 -1] [-1 0] [1 0] [0 1]]
       (map (partial pt/+ pt))
       (filter (comp not (partial contains? #{\# nil}) (partial grid/get test-terrain)))))
(defn elevation
  [pt]
  (let [t (grid/get test-terrain pt)]
    (or ((zipmap [\S \E \!] (repeat 1)) t) (Character/digit ^char t 10))))
(defn cost
  [pt-a pt-b]
  (apply pt/manhattan (map #(conj % (elevation %)) [pt-a pt-b])))

(deftest dij-build
  (testing "dij/build"
    (testing "Without cost fn"
      (testing "Path from start to end"
        (let [{dist :dist} (dij/build start neighbours)]
          (is (= (dist end) 9))
          (is (zero? (dist start)))
          (is (= (count dist) (- terrain-size num-obstacles 1))))) ;; 1 for the stuck position that we can't reach

      (testing "Path from a stuck position"
        (let [{dist :dist} (dij/build stuck neighbours)]
          (is (not (contains? dist end)))
          (is (= (count dist) 1)))))

    (testing "With cost fn"
      (testing "Path from start to end"
        (let [{dist :dist} (dij/build start neighbours cost)]
          (is (= (dist end) 19))
          (is (zero? (dist start)))
          (is (= (count dist) (- terrain-size num-obstacles 1))))) ;; 1 for the stuck position that we can't reach

      (testing "Path from a stuck position"
        (let [{dist :dist} (dij/build stuck neighbours cost)]
          (is (not (contains? dist end)))
          (is (= (count dist) 1)))))))

(deftest dij-path
  (testing "dij/path"
    (testing "Without cost fn"
      (testing "Path from start to end"
        (let [{prev :prev} (dij/build start neighbours)
              actual (dij/path start end prev)]
          (is (= (count actual) 10))
          (is (= start (first actual)))
          (is (= end (last actual)))))

      (testing "Path from a stuck position"
        (let [{prev :prev} (dij/build stuck neighbours)
              actual (dij/path stuck end prev)]
          (is (nil? actual)))))

    (testing "With cost fn"
      (let [{prev :prev} (dij/build start neighbours cost)
            actual (dij/path start end prev)]
        (is (= (count actual) 12))
        (is (= start (first actual)))
        (is (= end (last actual)))))))
