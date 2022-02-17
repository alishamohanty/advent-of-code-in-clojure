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
                   (map #(str/includes? % ".") row))
   :row-col row})

(defn bingo-row-col [row-cols]
  (if (true? (:bingo? row-cols))
    row-cols
    nil))

;;check if an entire row/column is marked?
;;if yes then bingo
(defn bingo? [board]
  (let [columns (transpose board)
        row-marked (keep bingo-row-col (mapv check-row-or-col-marked board))
        #_ (println "row marked" row-marked)
        col-marked (keep bingo-row-col (mapv check-row-or-col-marked columns))
        #_ (println "col marked" col-marked)]
    (if (or (not-empty row-marked) (not-empty col-marked))
      (if (not-empty row-marked)
        (select-keys (first row-marked) [:row-col])
        (select-keys (first col-marked) [:row-col]))
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

;;it takes boards and idx, when bingo is found, return idx
(defn check-bingo?
  ([boards] (check-bingo? boards 0))
  ([boards idx]
   (if-let [board (nth boards idx nil)]
     (if-let [bingo-row-col (bingo? board)]
       (assoc bingo-row-col :board-idx idx)
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
     :boards boards
     :itr input}))

(defn already-checked-bingo? [bingo-infos recent-bingo-info]
  (let [#_ (println "already-checked-bingo? :-" (some #(= (:board-idx %) (:board-idx recent-bingo-info)) bingo-infos))]
    (some #(= (:board-idx %) (:board-idx recent-bingo-info)) bingo-infos)))

(defn get-new-bingo
  ([boards bingo-infos bingos] (get-new-bingo boards bingo-infos bingos 0))
  ([boards bingo-infos bingos idx]
   (let [_ (println idx)]
     (if-let [board (nth boards idx nil)]
     (if-let [recent-bingo-info (bingo? board)]
       (let [_(println "recent-bingo-info" recent-bingo-info)]
         (if (already-checked-bingo? bingo-infos (assoc recent-bingo-info :board-idx idx))
           (get-new-bingo boards bingo-infos (inc idx))
           (do (println "hello")
               (get-new-bingo boards bingo-infos (conj bingos (assoc recent-bingo-info :board-idx idx)) (inc idx)))))
       (get-new-bingo boards bingo-infos (inc idx)))
     bingos))))


(defn not-already? [old-bingo-infos recent-bingo-info]
  (when recent-bingo-info
    (let [recent-bingo-idx (:board-idx recent-bingo-info)
          already-board-bingo? (some #(= (:board-idx %) (:board-dx recent-bingo-idx)) old-bingo-infos)
          already-bingo-inserted? (some #(= (select-keys % [:row-col :board-idx]) (select-keys recent-bingo-info [:row-col :board-idx])) old-bingo-infos)]
      (if (or already-bingo-inserted? already-board-bingo?)
      false
      true))))

;; -------- Solution ----------

(defn solution-part-2-wraper [{:keys [boards itr] :as board-struct}]
  (if-let [num (first itr)]
    (let [marked-boards (mark-boards boards num)
          bingo-infos (or (:bingo-infos board-struct) [])
          #_ (println "this is marked board" marked-boards)
          #_ (println "result of check-bingo" (check-bingo? marked-boards))
          #_recent-bingo-info #_(check-already-bingo? marked-boards bingo-infos) ;;{:row-col ["11." "14." "17."], :idx 1}
          #_is-new-bingo-info? #_(not-already? bingo-infos recent-bingo-info)
          #_(println "bingo-board-idx" bingo-board-idx)
          #_(println "bingo-infos" bingo-infos)
          #_(println "recent bingo info" recent-bingo-info)
          #_ (println "is new bingo info" is-new-bingo-info?)
          new_bingo_info (get-new-bingo marked-boards bingo-infos [])
          _ (println "value of new bingo info" new_bingo_info)]
      (if new_bingo_info
        (solution-part-2-wraper (assoc board-struct
                                       :itr (rest itr)
                                       :boards marked-boards
                                       :bingo-infos (conj bingo-infos
                                                         (assoc new_bingo_info
                                                                :num num))))
        (solution-part-2-wraper (assoc board-struct
                                       :itr (rest itr)
                                       :boards marked-boards))))
    board-struct))

(defn solution-part-2 [{:keys [input boards bingo-infos]}]
  (let [_ (println bingo-infos)
        last-bingo-infos (last bingo-infos)
        last-bingo-board (nth boards (:board-idx last-bingo-infos))
        num (:num last-bingo-infos)
        #_(println "last-bingo-infos" last-bingo-infos "last-bingo-board" last-bingo-board)
        #_(println "input:- " input)
        to-be-unmarked-vec (subvec input (inc (.indexOf
                                               input
                                               num)))
        #_ (println "unmark vec "to-be-unmarked-vec)
        #_ (println (nth boards (:board-idx last-bingo-infos)))
        sum (sum-unmarked-nums last-bingo-board to-be-unmarked-vec)
        #_ (println "sum:- " sum, last-bingo-board, to-be-unmarked-vec )]
    (* sum (Integer/parseInt num))))


;; -------- Answer ------------
(-> "inputs/problem_4.txt"
    parse
    solution-part-2-wraper)
    

;; -------- Performace --------
