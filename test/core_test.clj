(ns core-test
  (:require [problem-1.core :as prob-1]
            [problem-2.part-1 :as prob-2-part-1]
            [problem-2.part-2 :as prob-2-part-2]
            [clojure.test :refer [testing deftest is]]))

(deftest problem-1
  (testing "Testing problem 1 part - 1"
    (is (= 7 (prob-1/solution-part-1
              [199 200 208 210 200 207 240 269 260 263]))))
  (testing "Testing problem 1 part - 2"
    (is (= 5 (prob-1/solution-part-2
              [199 200 208 210 200 207 240 269 260 263])))))

(deftest problem-2
  (testing "Testing problem 2 part - 1"
    (is (= 150 (prob-2-part-1/solution-part-1
              [["forward" 5]
               ["down" 5]
               ["forward" 8]
               ["up" 3]
               ["down" 8]
               ["forward" 2]]))))
  (testing "Testing problem 2 part - 2"
    (is (= 900 (prob-2-part-2/solution-part-2
                [["forward" 5]
                 ["down" 5]
                 ["forward" 8]
                 ["up" 3]
                 ["down" 8]
                 ["forward" 2]])))))
