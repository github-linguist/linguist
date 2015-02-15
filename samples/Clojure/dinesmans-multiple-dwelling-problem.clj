(ns rosettacode.dinesman
  (:use [clojure.core.logic]
        [clojure.tools.macro :as macro]))

; whether x is immediately above (left of) y in list s; uses pattern matching on s
(defne aboveo [x y s]
       ([_ _ (x y . ?rest)])
       ([_ _ [_ . ?rest]] (aboveo x y ?rest)))

; whether x is on a higher floor than y
(defne highero [x y s]
       ([_ _ (x . ?rest)] (membero y ?rest))
       ([_ _ (_ . ?rest)] (highero x y ?rest)))

; whether x and y are on nonadjacent floors
(defn nonadjacento [x y s]
  (conda
    ((aboveo x y s) fail)
    ((aboveo y x s) fail)
    (succeed)))

(defn dinesmano [rs]
  (macro/symbol-macrolet [_ (lvar)]
    (all
      (permuteo ['Baker 'Cooper 'Fletcher 'Miller 'Smith] rs)
      (aboveo _ 'Baker rs) ;someone lives above Baker
      (aboveo 'Cooper _ rs) ;Cooper lives above someone
      (aboveo 'Fletcher _ rs)
      (aboveo _ 'Fletcher rs)
      (highero 'Miller 'Cooper rs)
      (nonadjacento 'Smith 'Fletcher rs)
      (nonadjacento 'Fletcher 'Cooper rs))))

(let [solns (run* [q] (dinesmano q))]
  (println "solution count:" (count solns))
  (println "solution(s) highest to lowest floor:")
  (doseq [soln solns] (println " " soln)))
