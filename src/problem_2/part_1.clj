(ns problem-2.part-1
  (:require [core :as core]
            [clojure.string :as str]))

;;Problem statement
;; - Part 1
;;https://adventofcode.com/2021/day/2

(defn forward [distance position]
  (assoc position :x (+ (:x position) distance)))

(defn up [distance position]
  (assoc position :y (- (:y position) distance)))

(defn down [distance position]
  (assoc position :y (+ (:y position) distance)))

(defn move [position [direction distance]]
  (cond
    (= direction "forward") (forward distance position)
    (= direction "up") (up distance position)
    (= direction "down") (down distance position)))

(defn pilot [commands]
  (reduce move {:x 0 :y 0} commands))

;; --------- Input parsing ---------

(defn distance-in-int [input]
  (let [direction (first input)
        distance (java.lang.Integer/parseInt (second input))]
    [direction distance]))

(defn parse [input]
  (-> input
      (str/split #"\s")
      (distance-in-int)))

(defn parse-input [filename]
  (let [str-commands (core/read-input filename)]
    (mapv parse str-commands)))

;; --------- Solutions --------------

(defn solution-part-1 [commands]
  (let [{:keys [x y]} (pilot commands)]
    (* x y)))

;; --------- Answers ----------------

(def answer-part-1
  (-> "inputs/problem_2.txt"
      parse-input
      solution-part-1))
