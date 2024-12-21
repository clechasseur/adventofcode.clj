(ns ^:util io.github.clechasseur.adventofcode.test.util.string
  (:require [clojure.test :refer :all]
            [io.github.clechasseur.adventofcode.util.string :as str]))

(deftest safe-subs
  (testing "safe-subs"
    (testing "arity 1"
      (testing "with start value in bounds"
        (let [s "hello"
              expected "ello"
              actual (str/safe-subs s 1)]
          (is (= actual expected)))))
    (testing "with start value out of bounds"
      (let [s "hello"
            expected "hello"
            actual (str/safe-subs s -1)]
        (is (= actual expected))))

    (testing "arity 2"
      (testing "with start and end values in bounds"
        (let [s "hello"
              expected "ell"
              actual (str/safe-subs s 1 4)]
          (is (= actual expected))))
      (testing "with start value out of bounds and end value in bounds"
        (let [s "hello"
              expected "hell"
              actual (str/safe-subs s -1 4)]
          (is (= actual expected)))))
    (testing "with start value in bounds and end value out of bounds"
      (let [s "hello"
            expected "ello"
            actual (str/safe-subs s 1 8)]
        (is (= actual expected))))
    (testing "with start and end values out of bounds"
      (let [s "hello"
            expected "hello"
            actual (str/safe-subs s -1 8)]
        (is (= actual expected))))))
