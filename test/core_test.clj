(ns core-test
  (:require [problem-1.core :as prob-1]
            [problem-2.part-1 :as prob-2-part-1]
            [problem-2.part-2 :as prob-2-part-2]
            [problem-3.part-1 :as prob-3-part-1]
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

(deftest problem-3
  (testing "Testing problem 3 part - 1 : Binary Diagnostic"
    (is (= 198 (prob-3-part-1/solution-part-1
              ["00100"
               "11110"
               "10110"
               "10111"
               "10101"
               "01111"
               "00111"
               "11100"
               "10000"
               "11001"
               "00010"
               "01010"])))))