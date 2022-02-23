(ns problem-5.part-2
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

(defn diagonal-elements [[[x1 y1] [x2 y2]]]
  (if (or (= x1 x2) (= y1 y2))
    nil
    [[x1 y1] [x2 y2]]))

(defn get-coordinates [accum [[x1 y1][x2 y2]]]
  (cond (< x1 x2)(if (< y1 y2)
                   (get-coordinates (conj accum [(inc x1) (inc y1)]) [[(inc x1) (inc y1)] [x2 y2]])
                   (get-coordinates (conj accum [(inc x1) (dec y1)]) [[(inc x1) (dec y1)] [x2 y2]]))
    (> x1 x2) (if (< y1 y2)
                (get-coordinates (conj accum [(dec x1) (inc y1)]) [[(dec x1) (inc y1)] [x2 y2]])
                (get-coordinates (conj accum [(dec x1) (dec y1)]) [[(dec x1) (dec y1)] [x2 y2]]))
        :else accum))


(defn misc [accum [[x1 y1] [x2 y2]]]
  (let [acc (conj [] [x1 y1])
        result (get-coordinates acc [[x1 y1] [x2 y2]])]
    (concat accum result)))


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

(defn solution-part-2 [input]
  (let [hor-vert-coords (remove nil? (map sanitise input))
        diagonal-coords (remove nil? (map diagonal-elements input))
        result-hor-vert (reduce in-between-points [] hor-vert-coords)
        result-diagonal (reduce misc [] diagonal-coords)
        ans (filter #(> (second %) 1) 
                    (frequencies (concat result-hor-vert result-diagonal)))]
    (count ans)))

;; ------ Answer ------- 
#_(def answer 
  (-> "inputs/problem_5.txt"
      parse-input
      solution-part-2))

;; -------- Performace --------

;;"Elapsed time: 136.88325 msecs"
#_(time (-> "inputs/problem_5.txt"
          parse-input
          solution-part-2))

;;16 B
#_(mm/measure (-> "inputs/problem_5.txt"
                parse-input
                solution-part-2))
