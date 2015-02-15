(ns user
  (:require [clojure.contrib.generic.math-functions :as generic]))

;(def pi Math/PI)
(def pi (* 4 (atan 1)))
(def dtor (/ pi 180))
(def rtod (/ 180 pi))
(def radians (/ pi 4))
(def degrees 45)

(println (str (sin radians) " " (sin (* degrees dtor))))
(println (str (cos radians) " " (cos (* degrees dtor))))
(println (str (tan radians) " " (tan (* degrees dtor))))
(println (str (asin (sin radians) ) " " (* (asin (sin (* degrees dtor))) rtod)))
(println (str (acos (cos radians) ) " " (* (acos (cos (* degrees dtor))) rtod)))
(println (str (atan (tan radians) ) " " (* (atan (tan (* degrees dtor))) rtod)))
