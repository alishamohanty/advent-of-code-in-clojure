(ns core-test
  (:require [problem-1.core :as prob-1]
            [problem-2.part-1 :as prob-2-part-1]
            [problem-2.part-2 :as prob-2-part-2]
            [problem-3.part-1 :as prob-3-part-1]
            [problem-3.part-2 :as prob-3-part-2]
            [problem-4.part-1 :as prob-4-part-1]
            [problem-4.part-2 :as prob-4-part-2]
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
                 "01010"]))))
  (testing "Testing problem 3 part - 1 : Binary Diagnostic"
    (is (= 230 (prob-3-part-2/solution-part-2
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

(deftest problem-4
  (testing "Giant Squid game part - 1"
    (is (= 4512 (prob-4-part-1/solution-part-1
                 {:input ["7" "4" "9" "5" "11" "17" "23"
                          "2" "0" "14" "21" "24" "10" "16"
                          "13" "6" "15" "25" "12" "22" "18"
                          "20" "8" "19" "3" "26" "1"]
                  :boards [[["22" "13" "17" "11" "0"]
                            ["8" "2" "23" "4" "24"]
                            ["21" "9" "14" "16" "7"]
                            ["6" "10" "3" "18" "5"]
                            ["1" "12" "20" "15" "19"]]
                           [["3" "15" "0" "2" "22"]
                            ["9" "18" "13" "17" "5"]
                            ["19" "8" "7" "25" "23"]
                            ["20" "11" "10" "24" "4"]
                            ["14" "21" "16" "12" "6"]]
                           [["14" "21" "17" "24" "4"]
                            ["10" "16" "15" "9" "19"]
                            ["18" "8" "23" "26" "20"]
                            ["22" "11" "13" "6" "5"]
                            ["2" "0" "12" "3" "7"]]]}))))
  (testing "Giant Squid game part - 2"
    (is (= 1924 (prob-4-part-2/solution-part-2
                 {:input ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14"
                          "21" "24" "10" "16" "13" "6" "15" "25" 
                          "12" "22" "18" "20" "8" "19" "3" "26" "1"],
                  :boards [[["22" "13" "17" "11" "0"]
                            ["8" "2" "23" "4" "24"]
                            ["21" "9" "14" "16" "7"]
                            ["6" "10" "3" "18" "5"]
                            ["1" "12" "20" "15" "19"]]
                           [["3" "15" "0" "2" "22"]
                            ["9" "18" "13" "17" "5"]
                            ["19" "8" "7" "25" "23"]
                            ["20" "11" "10" "24" "4"]
                            ["14" "21" "16" "12" "6"]]
                           [["14" "21" "17" "24" "4"]
                            ["10" "16" "15" "9" "19"]
                            ["18" "8" "23" "26" "20"]
                            ["22" "11" "13" "6" "5"]
                            ["2" "0" "12" "3" "7"]]],
                  :itr ["7" "4" "9" "5" "11" "17" "23" "2" "0" "14"
                        "21" "24" "10" "16" "13" "6" "15" "25"
                        "12" "22" "18" "20" "8" "19" "3" "26" "1"]})))))
