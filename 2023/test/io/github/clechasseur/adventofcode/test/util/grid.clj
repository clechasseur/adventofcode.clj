(ns ^:util io.github.clechasseur.adventofcode.test.util.grid
  (:require [clojure.test :refer :all]
            [io.github.clechasseur.adventofcode.util.grid :as grid]))

(def test-vector
  [1 2 3])
(def test-2d-grid
  [[1 2 3]
   [4 5 6]
   [7 8 9]])
(def test-3d-grid
  [[[1 2 3]
    [4 5 6]
    [7 8 9]]
   [[3 2 1]
    [6 5 4]
    [9 8 7]]
   [[7 8 9]
    [4 5 6]
    [1 2 3]]])
(def test-string-grid
  ["123"
   "456"
   "789"])

(deftest grid-get
  (testing "grid/get"
    (testing "with vector"
      (testing "when x is in bounds"
        (let [actual (grid/get test-vector [1])]
          (is (= actual 2))))
      (testing "when x is out of bounds"
        (let [actual (grid/get test-vector [8])]
          (is (nil? actual)))))

    (testing "with 2D grid"
      (testing "when both x and y are in bounds"
        (let [actual (grid/get test-2d-grid [1 1])]
          (is (= actual 5))))
      (testing "when x is out of bounds"
        (let [actual (grid/get test-2d-grid [8 1])]
          (is (nil? actual))))
      (testing "when y is out of bounds"
        (let [actual (grid/get test-2d-grid [1 8])]
          (is (nil? actual)))))

    (testing "with 3D grid"
      (testing "when x, y and z are in bounds"
        (let [actual (grid/get test-3d-grid [1 1 1])]
          (is (= actual 5))))
      (testing "when x is out of bounds"
        (let [actual (grid/get test-3d-grid [8 1 1])]
          (is (nil? actual))))
      (testing "when y is out of bounds"
        (let [actual (grid/get test-3d-grid [1 8 1])]
          (is (nil? actual))))
      (testing "when z is out of bounds"
        (let [actual (grid/get test-3d-grid [1 1 8])]
          (is (nil? actual)))))

    (testing "with string grid"
      (testing "when both x and y are in bounds"
        (let [actual (grid/get test-string-grid [1 1])]
          (is (= actual \5))))
      (testing "when x is out of bounds"
        (let [actual (grid/get test-string-grid [8 1])]
          (is (nil? actual))))
      (testing "when y is out of bounds"
        (let [actual (grid/get test-string-grid [1 8])]
          (is (nil? actual)))))))

(deftest grid-locate
  (testing "grid/locate"
    (testing "with vector"
      (testing "Find odd numbers"
        (let [expected [[0] [2]]
              actual (grid/locate odd? test-vector)]
          (is (= actual expected))))
      (testing "Find a specific number"
        (let [expected [[2]]
              actual (grid/locate (partial = 3) test-vector)]
          (is (= actual expected))))
      (testing "Find nothing"
        (let [actual (grid/locate (constantly false) test-vector)]
          (is (empty? actual)))))

    (testing "with 2D grid"
      (testing "Find odd numbers"
        (let [expected [[0 0]       [2 0]
                              [1 1]
                        [0 2]       [2 2]]
              actual (grid/locate odd? test-2d-grid)]
          (is (= actual expected))))
      (testing "Find a specific number"
        (let [expected [[2 0]]
              actual (grid/locate (partial = 3) test-2d-grid)]
          (is (= actual expected))))
      (testing "Find nothing"
        (let [actual (grid/locate (constantly false) test-2d-grid)]
          (is (empty? actual)))))

    (testing "with 3D grid"
      (testing "Find odd numbers"
        (let [expected [[0 0 0]         [2 0 0]
                                [1 1 0]
                        [0 2 0]         [2 2 0]

                        [0 0 1]         [2 0 1]
                                [1 1 1]
                        [0 2 1]         [2 2 1]

                        [0 0 2]         [2 0 2]
                                [1 1 2]
                        [0 2 2]         [2 2 2]]
              expected (set expected)
              actual (set (grid/locate odd? test-3d-grid))]
          (is (= actual expected))))
      (testing "Find specific numbers"
        (let [expected [[2 0 0] [0 0 1] [2 2 2]]
              expected (set expected)
              actual (set (grid/locate (partial = 3) test-3d-grid))]
          (is (= actual expected))))
      (testing "Find nothing"
        (let [actual (grid/locate (constantly false) test-3d-grid)]
          (is (empty? actual)))))

    (testing "with string grid"
      (testing "Find odd numbers"
        (let [expected [[0 0]       [2 0]
                        [1 1]
                        [0 2]       [2 2]]
              actual (grid/locate (comp odd? #(Character/getNumericValue ^char %)) test-string-grid)]
          (is (= actual expected))))
      (testing "Find a specific number"
        (let [expected [[2 0]]
              actual (grid/locate (partial = \3) test-string-grid)]
          (is (= actual expected))))
      (testing "Find nothing"
        (let [actual (grid/locate (constantly false) test-string-grid)]
          (is (empty? actual)))))))

(deftest grid-locate-one
  (testing "grid/locate-one"
    (testing "Find a single point"
      (let [expected [0 2]
            actual (grid/locate-one (partial = 7) test-2d-grid)]
        (is (= actual expected))))
    (testing "Find no matching point"
      (let [actual (grid/locate-one neg? test-2d-grid)]
        (is (nil? actual))))
    (testing "Finding multiple matching points doesn't work"
      (is (thrown? AssertionError (grid/locate-one even? test-2d-grid))))))

(deftest grid-dims
  (testing "For vector"
    (let [actual (grid/dims test-vector)]
      (is (= actual 1))))
  (testing "For 2D grid"
    (let [actual (grid/dims test-2d-grid)]
      (is (= actual 2))))
  (testing "For 3D grid"
    (let [actual (grid/dims test-3d-grid)]
      (is (= actual 3))))
  (testing "For string grid"
    (let [actual (grid/dims test-string-grid)]
      (is (= actual 2))))
  (testing "For non-grid"
    (let [actual (grid/dims 2)]
      (is (= actual 0)))))

(deftest grid-transpose
  (testing "grid/transpose"
    (testing "with vector"
      (is (thrown? AssertionError (grid/transpose test-vector))))
    (testing "with 2D grid"
      (let [expected [[1 4 7]
                      [2 5 8]
                      [3 6 9]]
            actual (grid/transpose test-2d-grid)]
        (is (= actual expected))))
    (testing "with 3D grid"
      (is (thrown? AssertionError (grid/transpose test-3d-grid))))
    (testing "with string grid"
      (let [expected [[\1 \4 \7]
                      [\2 \5 \8]
                      [\3 \6 \9]]
            actual (grid/transpose test-string-grid)]
        (is (= actual expected))))
    (testing "with non-grid"
      (is (thrown? AssertionError (grid/transpose 2))))))
