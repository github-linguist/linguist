(ns example
  (:require [clojure.contrib.math :as math]))

; Length of integer in binary
; (copied from a private multimethod in clojure.contrib.math)
(defmulti #^{:private true} integer-length class)

(defmethod integer-length java.lang.Integer [n]
  (count (Integer/toBinaryString n)))
(defmethod integer-length java.lang.Long [n]
  (count (Long/toBinaryString n)))
(defmethod integer-length java.math.BigInteger [n]
  (count (.toString n 2)))

(defn sierpinski-triangle [order]
  (loop [size (math/expt 2 order)
         v    (math/expt 2 (- size 1))]
    (when (pos? size)
      (println
       (apply str (map #(if (bit-test v %) "*" " ")
		       (range (integer-length v)))))
      (recur
       (dec size)
       (bit-xor (bit-shift-left v 1) (bit-shift-right v 1))))))

(sierpinski-triangle 4)
