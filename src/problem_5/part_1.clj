(ns problem-5.part-1
  (:require [clojure.string :as str]
            [core :as core]
            [clj-memory-meter.core :as mm]))

;;if point 1 is after point 2, reorder the coordinates
(defn arrange [[[x1 y1] [x2 y2]]]
  (cond
    (and (= x1 x2) (< y1 y2)) [[x1 y1] [x2 y2]]
    (and (= x1 x2) (> y1 y2)) [[x1 y2] [x2 y1]]
    (and (= y1 y2) (< x1 x2)) [[x1 y1] [x2 y2]]
    (and (= y1 y2) (> x1 x2)) [[x2 y1] [x1 y2]]
    :else [[x1 y1] [x2 y2]]))

;;[[x1 y1] [x2 y2]]
;;result -> [col of vectors]
(defn in-between-points [result points]
  (let [[[x1 y1] [x2 y2]] (arrange points)
        res (if (= x1 x2)
              (mapv #(concat [] [x1 %]) (range y1 (inc y2)))
              (mapv #(concat [] [% y1]) (range x1 (inc x2))))]
    (concat result res)))

(defn sanitise [[[x1 y1] [x2 y2]]]
  (if (or (= x1 x2) (= y1 y2))
    [[x1 y1] [x2 y2]]
    nil))

;; ------ Parsing ---------

(defn str->int [str]
  (Integer/parseInt str))

(defn coordinates [line]
  (let [p (str/split line #" -> ")
        [x1 y1] (str/split (first p) #",")
        [x2 y2] (str/split (second p) #",")
        points [[(str->int x1) (str->int y1)]
                [(str->int x2) (str->int y2)]]]
    points))

(defn parse-input [filename]
  (let [lines (core/read-input filename)]
    (mapv coordinates lines)))

;; ------- Solution ------

(defn solution-part-1 [input]
  (let [sanitised-ip (remove nil? (map sanitise input))
        result (reduce in-between-points [] sanitised-ip)
        ans (filter #(> (second %) 1) (frequencies result))]
    (count ans)))

;; ------ Answer ------- 
(def answer 
  (-> "inputs/problem_5.txt"
      parse-input
      solution-part-1))

;; -------- Performace --------

;;"Elapsed time: 95.640083 msecs"
(time (-> "inputs/problem_5.txt"
          parse-input
          solution-part-1))

;;16 B
(mm/measure (-> "inputs/problem_5.txt"
                parse-input
                solution-part-1))
