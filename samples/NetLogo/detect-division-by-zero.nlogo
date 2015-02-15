;; Division by zero detection using CAREFULLY
;; The CAREFULLY clause exists in NetLogo since version 2.0
;;   In prior versions of NetLogo, you must examine the divisor prior to performing the division.
;;   The variables result, a, and b must all be previously created global, local, or agent -own'd variables.
;; NetLogo variables are dynamically typed, so we are assuming that a and b contain numbers.
;; (All numbers in NetLogo are double-precision floating-point numbers.)
;;   However, even if not numbers, the result is still the same: the carefully clause will
;; supress the run-time error and run the "commands if error" block, setting result to false.
;; this false value can be detected, to alter the rest of the course of the code
;;   This behavior is consistent with other NetLogo primitives, such as POSTIION, that report
;; FALSE, rather than a number, if the operation fails.
carefully
[ ;; commands to try to run
  set result a / b
]
[ ;; commands to run if an error occurs in the previous block.
  set result false
]
ifelse is-number? result
[ output-print (word a " / " b " = " result)
]
[ output-print (word a " / " b " is not calculable"
]
