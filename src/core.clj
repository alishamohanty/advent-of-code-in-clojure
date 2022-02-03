(ns core
  (:require [clojure.string :as str]))

(defn read-input [filename]
  (-> filename
      slurp
      (str/split #"\n")))
