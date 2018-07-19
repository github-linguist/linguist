(ns rosettacode.align-columns
  (:require [clojure.contrib.string :as str]))

(def data "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")

(def table (map #(str/split #"\$" %) (str/split-lines data)))

(defn col-width [n table] (reduce max (map #(try (count (nth % n))
                                               (catch Exception _  0))
                                           table)))
(defn spaces [n] (str/repeat n " "))
(defn add-padding
  "if the string is too big turncate it, else return a string with padding"
  [string width justification]
  (if (>= (count string) width) (str/take width string)
      (let [pad-len (int (- width (count string))) ;we don't want rationals
            half-pad-len (int (/ pad-len 2))]
        (case justification
              :right (str (spaces pad-len) string)
              :left  (str string (spaces pad-len))
              :center (str (spaces half-pad-len) string (spaces (- pad-len half-pad-len)))))))

(defn aligned-table
  "get the width of each column, then generate a new table with propper padding for eath item"
  ([table justification]
  (let [col-widths (map #(+ 2 (col-width % table)) (range (count(first table))))]
    (map
     (fn [row] (map #(add-padding %1 %2 justification) row col-widths))
     table))))

(defn print-table
  [table]
  (do (println)
      (print (str/join "" (flatten (interleave table (repeat "\n")))))))

(print-table (aligned-table table :center))
