(ns problem-4.part-1
  (:require [clojure.string :as str]
            [clj-memory-meter.core :as mm]))

;; Problem - 4 Part - 1
;;https://adventofcode.com/2021/day/4

;;-----Solution-----

(defn transpose [m]
  (apply mapv vector m))

;;["1." "2." "3." "4." "5."] -> true
;; ["1." "2." "3" "4." "5."] -> false
(defn check-row-or-col-marked [row]
  (every? #(true? %)
          (map #(str/includes? % ".") row)))

;;check if an entire row/column is marked?
;;if yes then bingo
(defn bingo? [board]
  (let [columns (transpose board)
        entire-row-marked? (some #(true? %) (mapv check-row-or-col-marked board))
        entire-column-marked? (some #(true? %) (mapv check-row-or-col-marked columns))]
    (if (or entire-row-marked? entire-column-marked?)
      true
      false)))

;;If the required num is found will concat . at the end
(defn mark
  ([board num] (mark board num 0))
  ([board num idx] (if-let [idx-row (nth board idx nil)]
                     (let [checked-row (mapv #(if (= % num)
                                                (str num ".")
                                                %) idx-row)]
                       (cons checked-row (mark board num (inc idx))))
                     (list))))

(defn mark-boards [boards num]
  (mapv #(mark % num) boards))

;;if the element is not marked add to accum
(defn add-marked-elem [accum elem]
  (if (str/includes? elem ".")
    accum
    (+ accum (Integer/parseInt elem))))

(defn sum-unmarked-nums [board]
  (reduce add-marked-elem 0 (flatten board)))

;;it takes boards and idx, when bingo is found, return idx
(defn check-bingo?
  ([boards] (check-bingo? boards 0))
  ([boards idx]
   (if-let [board (nth boards idx nil)]
     (if (bingo? board)
       idx
       (check-bingo? boards (inc idx)))
     nil)))

;;----------- Parsing ------------

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #"\n\n")))

;;["1 2 3 4 5"] -> ["1" "2" "3" "4" "5"]
(defn remove-empty-strings [board-1d-vec]
  (vec (remove #(= "" %) board-1d-vec)))

;;["1 2 3" "4 5 6"] => [["1" "2" "3"] ["4" "5" "6"]]
(defn build-bingo-board [str-ip]
  (let [vec-ip (mapv #(-> %
                          (str/split #" ")) str-ip)
        removed-str-ip (mapv remove-empty-strings vec-ip)]
    removed-str-ip))

(defn build-bingo-boards [inputs]
  (let [bingo-boards (map #(str/split % #"\n") inputs)]
    (mapv build-bingo-board bingo-boards)))

(defn parse [filename]
  (let [parsed-ip (read-input filename)
        input (str/split (first parsed-ip) #",") ;; 1,2,3,4,5...
        board-1d-vec (rest parsed-ip)
        boards (build-bingo-boards board-1d-vec)] ;;[["1" "2" "3"] ["4" "5" "6"] ["7" "8" "9"]]
    {:input input
     :boards boards}))

;; -------- Solution ----------

(defn solution-part-1 [{:keys [input boards]}]
  (if-let [num (first input)]
    (let [marked-boards (mark-boards boards num)
          bingo-board-idx (check-bingo? marked-boards)]
      (if bingo-board-idx
        (let [bingo-board (nth marked-boards bingo-board-idx)]
          (* (sum-unmarked-nums bingo-board) (Integer/parseInt num)))
        (solution-part-1 {:input (rest input)
                          :boards marked-boards})))
    nil))

;; -------- Answer ------------
(-> "inputs/problem_4.txt"
    parse)

;; -------- Performace --------

;;"Elapsed time: 41.080292 msecs"
(time (-> "inputs/problem_4.txt"
          parse
          solution-part-1))

;;24 Bytes
(mm/measure (-> "inputs/problem_4.txt"
                parse
                solution-part-1))
