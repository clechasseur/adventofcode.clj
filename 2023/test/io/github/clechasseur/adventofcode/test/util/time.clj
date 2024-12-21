(ns ^:util io.github.clechasseur.adventofcode.test.util.time
  (:require [clojure.test :refer :all]
            [io.github.clechasseur.adventofcode.util.time :as time]))

(deftest time-it
  (testing "Simple test with no optional parameter"
    (let [timed (time/time-it (identity 23))]
      (is (= 23 timed))))
  (testing "Simple test with all parameters"
    (let [timed (time/time-it (identity 42)
                              :pre "Prefix string"
                              :pre-f #(str "Prefix string with " 23)
                              :post "Postfix string"
                              :post-f #(str "Postfix string receiving result = " %)
                              :off false)]
      (is (= 42 timed)))))
