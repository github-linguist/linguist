(deftemplate longest
  (slot bound)             ; upper bound for the range of values to check
  (slot next (default 2))  ; next value that needs to be checked
  (slot start (default 1)) ; starting value of longest sequence
  (slot len (default 1))   ; length of longest sequence
)

(deffacts startup
  (query 27)
  (longest (bound 100000))
)

(deffunction hailstone-next
  (?n)
  (if (evenp ?n)
    then (div ?n 2)
    else (+ (* 3 ?n) 1)
  )
)

(defrule extend-sequence
  ?hail <- (hailstone $?sequence ?tail&:(> ?tail 1))
  =>
  (retract ?hail)
  (assert (hailstone ?sequence ?tail (hailstone-next ?tail)))
)

(defrule start-query
  (query ?num)
  =>
  (assert (hailstone ?num))
)

(defrule result-query
  (query ?num)
  (hailstone ?num $?sequence 1)
  =>
  (bind ?sequence (create$ ?num ?sequence 1))
  (printout t "Hailstone sequence starting with " ?num ":" crlf)
  (bind ?len (length ?sequence))
  (printout t "  Length: " ?len crlf)
  (printout t "  First four: " (implode$ (subseq$ ?sequence 1 4)) crlf)
  (printout t "  Last four: " (implode$ (subseq$ ?sequence (- ?len 3) ?len)) crlf)
  (printout t crlf)
)

(defrule longest-create-next-hailstone
  (longest (bound ?bound) (next ?next))
  (test (<= ?next ?bound))
  (not (hailstone ?next $?))
  =>
  (assert (hailstone ?next))
)

(defrule longest-check-next-hailstone
  ?longest <- (longest (bound ?bound) (next ?next) (start ?start) (len ?len))
  (test (<= ?next ?bound))
  ?hailstone <- (hailstone ?next $?sequence 1)
  =>
  (retract ?hailstone)
  (bind ?thislen (+ 2 (length ?sequence)))
  (if (> ?thislen ?len) then
    (modify ?longest (start ?next) (len ?thislen) (next (+ ?next 1)))
    else
    (modify ?longest (next (+ ?next 1)))
  )
)

(defrule longest-finished
  (longest (bound ?bound) (next ?next) (start ?start) (len ?len))
  (test (> ?next ?bound))
  =>
  (printout t "The number less than " ?bound " that has the largest hailstone" crlf)
  (printout t "sequence is " ?start " with a length of " ?len "." crlf)
  (printout t crlf)
)
