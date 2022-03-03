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

(defn misc [ip _]
  (let [input (if (vector? ip) ip [ip])
        data (dec-and-count-0 {:elem input :count-0 0 :result []} 0)
        count-0 (:count-0 data)
        data-2 (add-8-to-seq (:result data) count-0)]
    data-2))

(defn count-fish [ip]
  (count (reduce #(misc %1 %2) ip (range 0 80))))

(defn how-many-fish-produce [accum ip]
  (if (accum ip)
    (assoc accum :total (+ (:total accum) (accum ip)))
    (let [total-fish (count-fish ip)]
      (assoc accum :total (+ (:total accum) total-fish)
             ip total-fish))))

;;;;;;;;; solution ;;;;;;;;

(defn solution [input]
  (let [c (reduce how-many-fish-produce {:total 0} input)]
    (:total c)))

;;;;;;;;; parsing ;;;;;;;;;

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #",")))

(defn parse-input [ip]
  (mapv #(Integer/parseInt %) ip))

;;;;;;;;; answer ;;;;;;;;;;;

(def answer-part-1
  (-> "inputs/problem_6.txt"
      read-input
      parse-input
      solution))
