(def x "") ;x is "globally" declared to be the empty string
(let [x ""]
  ;x is bound to the empty string within the let
  )
(= x "")    ;true if x is the empty string
(not= x "") ;true if x is not the empty string
