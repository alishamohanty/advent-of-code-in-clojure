(ns core-test
  (:require [problem-1.core :as prob-1]
            [clojure.test :refer [testing deftest is]]))

(deftest problem-1
  (testing "Testing problem 1 part - 1"
    (is (= 7 (prob-1/solution-part-1 
              [199 200 208 210 200 207 240 269 260 263]))))
  (testing "Testing problem 1 part - 2"
    (is (= 5 (prob-1/solution-part-2 
              [199 200 208 210 200 207 240 269 260 263])))))
