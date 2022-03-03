(ns problem-6.part-2
  (:require [clojure.string :as str]
            [clj-memory-meter.core :as mm]))

(def base-struct
  {0 0
   1 0
   2 0
   3 0
   4 0
   5 0
   6 0
   7 0
   8 0})

(defn create-structure [base-struct ip]
  (assoc base-struct ip 1))

(defn decrease [struct _]
  {0 (get struct 1)
   1 (get struct 2)
   2 (get struct 3)
   3 (get struct 4)
   4 (get struct 5)
   5 (get struct 6)
   6 (+ (get struct 0) (get struct 7))
   7 (get struct 8)
   8 (get struct 0)})

(defn calculate-fishes [struct]
  (reduce #(+ %1 (second %2)) 0 struct))

(defn total-fishes-for-256-days [ip]
  (let [initial-struct (create-structure base-struct ip)
        ans-struct (reduce decrease initial-struct (range 0 256))]
    (calculate-fishes ans-struct)))

(defn solution [ip]
  (reduce #(+ %1 (total-fishes-for-256-days %2)) 0 ip))

;;;;;;;;;; parsing ;;;;;;;;;;

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #",")))

(defn parse-input [ip]
  (mapv #(Integer/parseInt %) ip))

;;;;;;;;;; answer ;;;;;;;;;;;

(def answer-part-2
  (-> "inputs/problem_6.txt"
      read-input
      parse-input
      solution))

;; -------- Performace --------

;;"Elapsed time: 24.93 msecs"
(time (-> "inputs/problem_6.txt"
            read-input
            parse-input
            solution))

;;24 B
(mm/measure (-> "inputs/problem_6.txt"
                  read-input
                  parse-input
                  solution))
