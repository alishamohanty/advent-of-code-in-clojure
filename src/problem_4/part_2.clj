(ns problem-4.part-2
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
  {:bingo? (every? #(true? %)
                   (map #(str/includes? % ".") row))})

(defn bingo-row-col [row-cols]
  (if (true? (:bingo? row-cols))
    row-cols
    nil))

;;check if an entire row/column is marked?
;;if yes then bingo
(defn bingo? [board]
  (let [columns (transpose board)
        row-marked (keep bingo-row-col (mapv check-row-or-col-marked board))
        col-marked (keep bingo-row-col (mapv check-row-or-col-marked columns))]
    (if (or (not-empty row-marked) (not-empty col-marked))
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
(defn remove-dot [num]
  (apply str (drop-last num)))

(defn clean [num]
  (if (str/includes? num ".")
    (remove-dot num)
    num))

(defn find-elem [elem aft-list]
  (some #(= (remove-dot elem) %) aft-list))

(defn sum-unmarked-nums [board aft-list]
  (let [add-marked-elem (fn [accum elem]
                          (if (and (str/includes? elem ".")
                                   (not (find-elem elem aft-list)))
                            accum
                            (+ accum (Integer/parseInt (clean elem)))))]
    (reduce add-marked-elem 0 (flatten board))))

(defn already-checked-bingo? [bingo-infos recent-bingo-info]
  (some #(= (:board-idx %) (:board-idx recent-bingo-info)) bingo-infos))

(defn get-new-bingo
  ([boards bingo-infos new-bingos] (get-new-bingo boards bingo-infos new-bingos 0))
  ([boards bingo-infos new-bingos idx]
   (if-let [board (nth boards idx nil)]
     (if (bingo? board)
       (if (already-checked-bingo? bingo-infos (assoc {} :board-idx idx))
         (get-new-bingo boards bingo-infos new-bingos (inc idx))
         (get-new-bingo boards bingo-infos (conj new-bingos (assoc {} :board-idx idx)) (inc idx)))
       (get-new-bingo boards bingo-infos new-bingos (inc idx)))
     new-bingos)))

(defn append [num bingo-infos]
  (map #(assoc % :num num) bingo-infos))

(defn mark-boards-from-input [{:keys [boards itr] :as board-struct}]
  (if-let [num (first itr)]
    (let [marked-boards (mark-boards boards num)
          bingo-infos (or (:bingo-infos board-struct) [])
          new_bingo_info (get-new-bingo marked-boards bingo-infos [])]
      (if (not-empty new_bingo_info)
        (mark-boards-from-input (assoc board-struct
                                       :itr (rest itr)
                                       :boards marked-boards
                                       :bingo-infos (concat bingo-infos
                                                            (append num new_bingo_info))))
        (mark-boards-from-input (assoc board-struct
                                       :itr (rest itr)
                                       :boards marked-boards))))
    board-struct))


(defn solution-part-2 [input-boards]
  (let [{:keys [input boards bingo-infos]} (mark-boards-from-input input-boards)
        last-bingo-infos (last bingo-infos)
        last-bingo-board (nth boards (:board-idx last-bingo-infos))
        num (:num last-bingo-infos)
        to-be-unmarked-vec (subvec input (inc (.indexOf
                                               input
                                               num)))
        sum (sum-unmarked-nums last-bingo-board to-be-unmarked-vec)]
    (* sum (Integer/parseInt num))))

;; ------Parsing ----

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
     :boards boards
     :itr input}))

;; -------- Answer ------------
#_(-> "inputs/problem_4.txt"
    parse
    solution-part-2)

;; -------- Performace --------
;;"Elapsed time: 110.872625 msecs"
#_(time (-> "inputs/problem_4.txt"
          parse
          solution-part-2))

;;24B
#_(mm/measure (-> "inputs/problem_4.txt"
                parse
                solution-part-2))
