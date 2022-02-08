(ns problem-3.part-1
  (:require [core :as core]
            [clj-memory-meter.core :as mm]))

(defn bits->number [bits]
  (Integer/parseInt bits 2))

(defn initialise-count [bits-data]
  (take (count bits-data) (repeat 0)))

(defn helper [count-of-1 bits]
  (map-indexed (fn [key val]
                 (if (= (nth bits key) \1)
                   (inc val)
                   val)) count-of-1))

(defn count-ones [bits-seq]
  (let [counts-of-1 (reduce helper
                            (initialise-count (first bits-seq))
                            bits-seq)
        total (count bits-seq)]
    {:counts-of-1 (vec counts-of-1)
     :total total
     :diagnostic-ips bits-seq}))


(defn oxygen-rating-wrapper [{:keys [counts-of-1 total diagnostic-ips] :as struct} iteration result nth-idx]
  (let [nth-diagnostic-ip (nth diagnostic-ips iteration nil)
        #_(println "iteration number: " iteration)]
    (if nth-diagnostic-ip
      (let [count-1-of-nth-bit (nth counts-of-1 nth-idx)
            count-0-of-nth-bit (- total count-1-of-nth-bit)
            #_(println "nth-bits: " nth-diagnostic-ip "\ncount-1-of-nth-bit: " count-1-of-nth-bit)
            most-common-bit (if (>= count-1-of-nth-bit count-0-of-nth-bit)
                              \1
                              \0)
            #_ (println "most common bit is: " most-common-bit)
            nth-bit-diagnostic-ip (nth nth-diagnostic-ip nth-idx)
            #_ (println "value of nth bit i.e " nth-idx " bit is" nth-bit-diagnostic-ip)
            #_ (println "value of result" result)
            #_ (println "value of if is" (= nth-bit-diagnostic-ip most-common-bit))]
        (if (= nth-bit-diagnostic-ip most-common-bit)
          (oxygen-rating-wrapper struct (inc iteration) (conj result nth-diagnostic-ip) nth-idx)
          (oxygen-rating-wrapper struct (inc iteration) result nth-idx)))
      result)))

(defn oxygen-gen-rating
  ([diagnostic-ips] (oxygen-gen-rating (count-ones diagnostic-ips) (count diagnostic-ips) 0))
  ([struct len idx] (if (> len 1)
                      (let [new-struct (count-ones (oxygen-rating-wrapper struct 0 [] idx))]
                        (oxygen-gen-rating new-struct (:total new-struct) (inc idx)))
                      struct)))


(defn co2-rating-wrapper [{:keys [counts-of-1 total diagnostic-ips] :as struct} iteration result nth-idx]
  (let [nth-diagnostic-ip (nth diagnostic-ips iteration nil)
        #_(println "iteration number: " iteration)]
    (if nth-diagnostic-ip
      (let [count-1-of-nth-bit (nth counts-of-1 nth-idx)
            count-0-of-nth-bit (- total count-1-of-nth-bit)
            #_(println "nth-diagnostic-ip: " nth-diagnostic-ip "\ncount-1-of-nth-bit: " count-1-of-nth-bit)
            least-common-bit (if (> count-0-of-nth-bit count-1-of-nth-bit)
                              \1
                              \0)
            #_(println "least common bit is: " least-common-bit)
            nth-bit-diagnostic-ip (nth nth-diagnostic-ip nth-idx)
            #_(println "value of " nth-idx " of nth bits" nth-bit-diagnostic-ip)
            #_(println "value of result" result)]
        (if (= nth-bit-diagnostic-ip least-common-bit)
          (co2-rating-wrapper struct (inc iteration) (conj result nth-diagnostic-ip) nth-idx)
          (co2-rating-wrapper struct (inc iteration) result nth-idx)))
      result)))


(defn co2-gen-rating
  ([diagnostic-ips] (co2-gen-rating (count-ones diagnostic-ips) (count diagnostic-ips) 0))
  ([struct len idx] (if (> len 1)
                      (let [new-struct (count-ones (co2-rating-wrapper struct 0 [] idx))]
                        (co2-gen-rating new-struct (:total new-struct) (inc idx)))
                      struct)))

;;-----
(defn parse-input [filename]
  (core/read-input filename))

(defn solution-part-2 [diagnostic-ips]
  (let [o2-rating-bits (first (:diagnostic-ips (oxygen-gen-rating diagnostic-ips)))
        co2-rating-bits (first (:diagnostic-ips (co2-gen-rating diagnostic-ips)))]
    (* (bits->number o2-rating-bits) (bits->number co2-rating-bits))))

(def answer-part-2
  (-> "inputs/problem_3.txt"
      parse-input
      solution-part-2))