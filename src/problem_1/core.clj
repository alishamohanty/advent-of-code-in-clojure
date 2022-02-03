(ns problem-1.core
  (:require [core :as core]))

;;Problem statement
;;https://adventofcode.com/2021/day/1

(defn count-inc-in-depths
  ([depths] (count-inc-in-depths (first depths)
                                 (rest depths)
                                 0))
  ([fir rest-seq count]
   (cond
     (empty? rest-seq) count
     (< fir (first rest-seq)) (count-inc-in-depths (first rest-seq)
                                                   (rest rest-seq)
                                                   (inc count))
     :else (count-inc-in-depths (first rest-seq)
                                (rest rest-seq)
                                count))))

(defn convert-window-of-3 [depths]
  (->> depths
       (partition 3 1)
       (mapv #(apply + %))))

;; ---------- Solution -------------

(defn solution-part-1 [depths]
  (-> depths
      count-inc-in-depths))

(defn solution-part-2 [depths]
  (-> depths
      convert-window-of-3
      count-inc-in-depths))

;; ---------- Answer -------------

(defn- parse-input [filename]
  (let [str-depths (core/read-input filename)]
    (mapv #(java.lang.Integer/parseInt %) str-depths)))

(def answer-part-1 
  (-> "inputs/problem_1.txt"
      parse-input
      solution-part-1))

(def answer-part-2
  (-> "inputs/problem_1.txt"
      parse-input
      solution-part-2))
