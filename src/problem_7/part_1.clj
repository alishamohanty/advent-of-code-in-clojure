(ns problem-7.part-1
  (:require [clojure.string :as str]
            [clj-memory-meter.core :as mm]))

(defn total-distance [start points]
  (reduce #(+ %1 (Math/abs (- start %2))) 0 points))

(defn solution [points]
  (let [points-freq (frequencies points)
        unique-points (map (fn [[key _]] key) points-freq)
        distances (mapv (fn [start] (total-distance start
                                                    points)) unique-points)
        result (apply min distances)]
    result))

;;;;;;;;;;; Parsing ;;;;;;;;;;;;

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #",")))

(defn parse-input [filename]
  (let [input (read-input filename)]
    (mapv #(Integer/parseInt %) input)))

;;;;;;;;;;; Answer ;;;;;;;;;;;;;;

(def answer-part-1 
  (-> "inputs/problem_7.txt"
      parse-input
      solution))

;;;;;;;;;;; Performance ;;;;;;;;;;;;

;; The solution time complexity is bad. O(n^2)
;;"Elapsed time: 1170.94125 msecs"
(time (-> "inputs/problem_7.txt"
          parse-input
          solution))

;;24 B
(mm/measure (-> "inputs/problem_7.txt"
                parse-input
                solution))
