(ns problem-3.part-2
  (:require [core :as core]
            [clj-memory-meter.core :as mm]))

;; Problem - 3 Part - 2
;;https://adventofcode.com/2021/day/3

(defn bits->number [bits]
  (Integer/parseInt bits 2))

(defn initialise-count [bits-data]
  (take (count bits-data) (repeat 0)))

(defn helper [count-of-1 bits]
  (map-indexed (fn [key val]
                 (if (= (nth bits key) \1)
                   (inc val)
                   val)) count-of-1))

;; ["101" "111" "011"] -> 
#_{:counts-of-1 [2 2 3]
   :total 3
   :diagnostic-ips ["101" "111" "011"]}
(defn build-diagnostic-struct [bits-seq]
  (let [counts-of-1 (reduce helper
                            (initialise-count (first bits-seq))
                            bits-seq)
        total (count bits-seq)]
    {:counts-of-1 (vec counts-of-1)
     :total total
     :diagnostic-ips bits-seq}))

;; Oxygen-rating-wrapper 
;;Given a particular index, it checks all the bits-seqs and return sequences which match the most-common-bit
;;Solves this recursively. In each iteration it checks if the current string ("101") has the
;;same most common bit at i-th index and then calls the next iteration
;;Example :-
#_{:counts-of-1 [2 1 2]
   :total 3
   :diagnostic-ips ["101" "100" "011"]}
;; iteration: 0 result: ["101"] nth-index: 0 
;;So it checks 0th index of all the i/p and return the seq with has i-th bit same as most common bit 
(defn oxygen-rating-wrapper [{:keys [counts-of-1
                                     total
                                     diagnostic-ips] :as struct}
                             iteration
                             result
                             nth-idx]
  (let [nth-diagnostic-ip (nth diagnostic-ips iteration nil)] 
    (if nth-diagnostic-ip
      (let [count-1-of-nth-bit (nth counts-of-1 nth-idx)
            count-0-of-nth-bit (- total count-1-of-nth-bit)
            most-common-bit (if (>= count-1-of-nth-bit
                                    count-0-of-nth-bit)
                              \1
                              \0)
            nth-bit-diagnostic-ip (nth nth-diagnostic-ip nth-idx)]
        (if (= nth-bit-diagnostic-ip most-common-bit)
          (oxygen-rating-wrapper struct 
                                 (inc iteration)
                                 (conj result
                                       nth-diagnostic-ip)
                                 nth-idx)
          (oxygen-rating-wrapper struct 
                                 (inc iteration)
                                 result
                                 nth-idx)))
      result)))

;; Oxygen-gen-rating

;;It calls the wrapper to get result diagnostic sequences for n number of bits of each i/p
;;It will recursively call it for n times / until only one i/p is left

;; For this ["101" "100" "011"] -> 

;;1st iteration for first bit - [101 100] {:counts-of-1 [2 1 2], :total 3, :diagnostic-ips [101 100 011]}
;;2nd iteration for second bit - [101 100] {:counts-of-1 [2 0 1], :total 2, :diagnostic-ips [101 100]}
;;3rd iteration for third bit - [101 100] {:counts-of-1 [2 0 1], :total 2, :diagnostic-ips [101 100]}
;;Final result #_{:counts-of-1 [1 0 1], :total 1, :diagnostic-ips ["101"]}

(defn oxygen-gen-rating
  ([diagnostic-ips] (oxygen-gen-rating (build-diagnostic-struct diagnostic-ips) (count diagnostic-ips) 0))
  ([struct len idx] (if (> len 1)
                      (let [result (oxygen-rating-wrapper struct 0 [] idx)
                            new-struct (build-diagnostic-struct result)]
                        (oxygen-gen-rating new-struct 
                                           (:total new-struct)
                                           (inc idx)))
                      struct)))

 ;; Similar for co2 rating
(defn co2-rating-wrapper [{:keys [counts-of-1 
                                  total
                                  diagnostic-ips] :as struct} 
                          iteration
                          result
                          nth-idx]
  (let [nth-diagnostic-ip (nth diagnostic-ips iteration nil)]
    (if nth-diagnostic-ip
      (let [count-1-of-nth-bit (nth counts-of-1 nth-idx)
            count-0-of-nth-bit (- total count-1-of-nth-bit)
            least-common-bit (if (> count-0-of-nth-bit count-1-of-nth-bit)
                              \1
                              \0)
            nth-bit-diagnostic-ip (nth nth-diagnostic-ip nth-idx)]
        (if (= nth-bit-diagnostic-ip least-common-bit)
          (co2-rating-wrapper struct (inc iteration) (conj result nth-diagnostic-ip) nth-idx)
          (co2-rating-wrapper struct (inc iteration) result nth-idx)))
      result)))

(defn co2-gen-rating
  ([diagnostic-ips] (co2-gen-rating (build-diagnostic-struct diagnostic-ips) (count diagnostic-ips) 0))
  ([struct len idx] (if (> len 1)
                      (let [new-struct (build-diagnostic-struct (co2-rating-wrapper struct 0 [] idx))]
                        (co2-gen-rating new-struct (:total new-struct) (inc idx)))
                      struct)))

;;------------ Parsing ---------
(defn parse-input [filename]
  (core/read-input filename))

;; ------- Solution ------------
(defn solution-part-2 [diagnostic-ips]
  (let [o2-rating-bits (first (:diagnostic-ips (oxygen-gen-rating diagnostic-ips)))
        co2-rating-bits (first (:diagnostic-ips (co2-gen-rating diagnostic-ips)))]
    (* (bits->number o2-rating-bits) (bits->number co2-rating-bits))))

;;--------- Answer ------------

(def answer-part-2
  (-> "inputs/problem_3.txt"
      parse-input
      solution-part-2))

;; --------- Performace -----------

;;"Elapsed time: 6.726459 msecs"
#_(time (-> "inputs/problem_3.txt"
          parse-input
          solution-part-2))

;;"24 B"
#_(mm/measure (-> "inputs/problem_3.txt"
                parse-input
                solution-part-2))
