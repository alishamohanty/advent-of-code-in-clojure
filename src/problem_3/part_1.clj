(ns problem-3.part-1
  (:require [core :as core]))

;; Problem - 3 Part - 1
;;https://adventofcode.com/2021/day/3

;; "11011" -> 27
(defn bits->number [bits]
  (Integer/parseInt bits 2))

;;most common bit
(defn gamma-rate [{:keys [counts-of-1 total]}]
  (reduce (fn [gamma count]
            (if (> count (- total count))
              (str gamma "1")
              (str gamma "0"))) "" counts-of-1))

;;least common bit
(defn alpha-rate [{:keys [counts-of-1 total]}]
  (reduce (fn [alpha count]
            (if (<= count (- total count))
              (str alpha "1")
              (str alpha "0"))) "" counts-of-1))

(defn initialise-count [bits-data]
  (take (count bits-data) (repeat 0)))

;;[2 0 0 0] 1011 -> [3 0 1 1]
(defn helper [count-of-1 bits]
  (map-indexed (fn [key val]
                 (if (= (nth bits key) \1)
                   (inc val)
                   val)) count-of-1))

;;count struct
#_{:counts-of-1 [1 2 3 1 2]
   :total 3}
(defn count-ones [bits-seq]
  (let [counts-of-1 (reduce helper
                            (initialise-count (first bits-seq))
                            bits-seq)
        total (count bits-seq)]
    {:counts-of-1 (vec counts-of-1)
     :total total}))

;; ------------ Parsing ----------------

(defn parse-input [filename]
  (core/read-input filename))

;; ------------ Solution ---------------

(defn solution-part-1 [bits-seq]
  (let [count-struct (-> bits-seq
                         count-ones)
        gamma (bits->number (gamma-rate count-struct))
        #_ (println "Gamma" gamma)
        alpha (bits->number (alpha-rate count-struct))
        #_ (println "Alpha" alpha)]
    (* gamma alpha)))

;; ----------- Answer ----------------

(def answer-part-1
  (-> "inputs/problem_3.txt"
      parse-input
      solution-part-1))
