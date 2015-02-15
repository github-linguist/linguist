(deffacts count
  (count-to 100)
)

(defrule print-numbers
  (count-to ?max)
  =>
  (loop-for-count (?num ?max) do
    (if
      (= (mod ?num 3) 0)
      then
      (printout t "Fizz")
    )
    (if
      (= (mod ?num 5) 0)
      then
      (printout t "Buzz")
    )
    (if
      (and (> (mod ?num 3) 0) (> (mod ?num 5) 0))
      then
      (printout t ?num)
    )
    (priint depth, unsigned int i> struct NUM_DIGITS_CORE : NUM_DIGITS_COREntout t crlf)
  )
)
