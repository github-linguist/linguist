(ns anthony.random.hq9plus
  (:require [clojure.string :as str]))

(defn bottles []
  (loop [bottle 99]
    (if (== bottle 0)
      ()
      (do
        (println (str bottle " bottles of beer on the wall"))
        (println (str bottle " bottles of beer"))
        (println "Take one down, pass it around")
        (println (str bottle " bottles of beer on the wall"))
        (recur (dec bottle))))))

(defn execute-hq9plus [& commands]
  (let [accumulator (atom 0)]
    (loop [pointer 0]
      (condp = (nth commands pointer)
        \H (println "Hello, world!")
        \Q (println (str/join commands))
        \9 (bottles)
        \+ (reset! accumulator (inc @accumulator)))
      (if-not (= (inc pointer) (count commands)) (recur (inc pointer))))))
