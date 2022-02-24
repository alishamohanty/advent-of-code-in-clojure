(ns problem-6.part-1
  (:require [clojure.string :as str]))

(defn add-8 [ip _]
  (conj ip 8))

(defn add-8-to-seq [ip count-0]
  (if count-0
    (reduce add-8 ip (range count-0))
    ip))

(defn dec-and-count-0 [{:keys [elem count-0 result] :as ans} itr]
  (if (< itr (count elem))
    (if (= (nth elem itr) 0) 
      (dec-and-count-0 (assoc ans
                :result (conj result 6)
                :count-0 (inc count-0)) (inc itr))
      (dec-and-count-0 (assoc ans
                :result (conj result (dec (nth elem itr)))) (inc itr)))
    ans))

;;new lanternfish with an internal timer of 8
(defn misc [ip]
  (let [data (dec-and-count-0 {:elem ip :count-0 0 :result []} 0)
        count-0 (:count-0 data)
        data-2 (add-8-to-seq (:result data) count-0)]
    data-2))

(defn func [accum _]
  (misc accum))

;;
(defn solution [input]
  (let [total (reduce func input (range 0 80))]
    (count total)))

;;what did i do?
;;first minus every thing except if has 0

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #",")))

(defn parse-input [ip]
  (mapv #(Integer/parseInt %) ip))

(def answer-part-1
  (-> "inputs/problem_6.txt"
      read-input
      parse-input
      solution))

;;usual way is stack overflow