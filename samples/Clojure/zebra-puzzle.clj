(ns zebra.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [clojure.tools.macro :as macro]))

(defne lefto [x y l]
       ([_ _ [x y . ?r]])
       ([_ _ [_ . ?r]] (lefto x y ?r)))

(defn nexto [x y l]
  (conde
    ((lefto x y l))
    ((lefto y x l))))

(defn zebrao [hs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
      (== [_ _ _ _ _] hs)
      (membero ['englishman _ _ _ 'red] hs)
      (membero ['swede _ _ 'dog _] hs)
      (membero ['dane _ 'tea _ _] hs)
      (lefto [_ _ _ _ 'green] [_ _ _ _ 'white] hs)
      (membero [_ _ 'coffee _ 'green] hs)
      (membero [_ 'pallmall _ 'birds _] hs)
      (membero [_ 'dunhill _ _ 'yellow] hs)
      (== [_ _ [_ _ 'milk _ _] _ _ ] hs)
      (firsto hs ['norwegian _ _ _ _])
      (nexto [_ 'blend _ _ _] [_ _ _ 'cats _ ] hs)
      (nexto [_ _ _ 'horse _] [_ 'dunhill _ _ _] hs)
      (membero [_ 'bluemaster 'beer _ _] hs)
      (membero ['german 'prince _ _ _] hs)
      (nexto ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)
      (nexto [_ _ 'water _ _] [_ 'blend _ _ _] hs)
      (membero [_ _ _ 'zebra _] hs))))

(let [solns (run* [q] (zebrao q))
      soln (first solns)
      zebra-owner (->> soln (filter #(= 'zebra (% 3))) first (#(% 0)))]
  (println "solution count:" (count solns))
  (println "zebra owner is the" zebra-owner)
  (println "full solution (in house order):")
  (doseq [h soln] (println " " h)))
