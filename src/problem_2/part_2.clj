(ns problem-2.part-2
  (:require [core :as core]
            [clojure.string :as str]))

;;Problem statement
;; - Part 2
;;https://adventofcode.com/2021/day/2

(defn forward [X {:keys [aim position] :as coordinates}]
  (let [pos-x (+ (:x position) X)
        pos-y (+ (:y position) (* aim X))
        new-pos (assoc position 
                       :x pos-x
                       :y pos-y)]
    (assoc coordinates
         :position new-pos)))

(defn up [X {:keys [aim] :as coordinates}]
  (assoc coordinates :aim (- aim X)))

(defn down [X {:keys [aim] :as coordinates}]
  (assoc coordinates :aim (+ aim X)))

(defn move [coordinates [direction X]]
  (cond
    (= direction "forward") (forward X coordinates)
    (= direction "up") (up X coordinates)
    (= direction "down") (down X coordinates)))

(defn pilot [commands]
  (reduce move {:position {:x 0 :y 0}
                :aim 0} commands))

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

(defn solution-part-2 [commands]
  (let [{:keys [x y]} (:position (pilot commands))]
    (* x y)))

;; --------- Answers ----------------

(def answer-part-2
  (-> "inputs/problem_2.txt"
      parse-input
      solution-part-2))
