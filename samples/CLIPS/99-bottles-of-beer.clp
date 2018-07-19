(deffacts beer-bottles
  (bottles 99))

(deffunction bottle-count
  (?count)
  (switch ?count
    (case 0 then "No more bottles of beer")
    (case 1 then "1 more bottle of beer")
    (default (str-cat ?count " bottles of beer"))))

(defrule stanza
  ?bottles <- (bottles ?count)
  =>
  (retract ?bottles)
  (printout t (bottle-count ?count) " on the wall," crlf)
  (printout t (bottle-count ?count) "." crlf)
  (printout t "Take one down, pass it around," crlf)
  (printout t (bottle-count (- ?count 1)) " on the wall." crlf crlf)
  (if (> ?count 1) then (assert (bottles (- ?count 1)))))
