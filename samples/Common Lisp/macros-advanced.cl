;; @file macros-advanced.cl
;;
;; @breif Advanced macro practices - defining your own macros
;;
;; Macro definition skeleton:
;; (defmacro name (parameter*)
;;   "Optional documentation string"
;;   body-form*)
;;
;; Note that backquote expression is most often used in the `body-form`
;;

; `primep` test a number for prime
(defun primep (n)
  "test a number for prime"
  (if (< n 2) (return-from primep))
  (do ((i 2 (1+ i)) (p t (not (zerop (mod n i)))))
      ((> i (sqrt n)) p)
    (when (not p) (return))))
; `next-prime` return the next prime bigger than the specified number
(defun next-prime (n)
  "return the next prime bigger than the speicified number"
  (do ((i (1+ n) (1+ i)))
      ((primep i) i)))
;
; The recommended procedures to writting a new macro are as follows:
; 1. Write a sample call to the macro and the code it should expand into
(do-primes (p 0 19)
  (format t "~d " p))
; Expected expanded codes
(do ((p (next-prime (- 0 1)) (next-prime p)))
    ((> p 19))
  (format t "~d " p))
; 2. Write code that generate the hardwritten expansion from the arguments in
; the sample call
(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime (- ,start 1)) (next-prime ,var)))
         ((> ,var ,end))
      ,@body)))
; 2-1. More concise implementations with the 'parameter list destructuring' and
; '&body' synonym, it also emits more friendly messages on incorrent input.
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime (- ,start 1)) (next-prime ,var)))
       ((> ,var ,end))
    ,@body))
; 2-2. Test the result of macro expansion with the `macroexpand-1` function
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))
; 3. Make sure the macro abstraction does not "leak"
(defmacro do-primes ((var start end) &body body)
  (let ((end-value-name (gensym)))
    `(do ((,var (next-prime (- ,start 1)) (next-prime ,var))
          (,end-value-name ,end))
         ((> ,var ,end-value-name))
      ,@body)))
; 3-1. Rules to observe to avoid common and possible leaks
;   a. include any subforms in the expansion in positions that will be evaluated
;      in the same order as the subforms appear in the macro call
;   b. make sure subforms are evaluated only once by creating a variable in the
;      expansion to hold the value of evaluating the argument form, and then
;      using that variable anywhere else the value is needed in the expansion
;   c. use `gensym` at macro expansion time to create variable names used in the
;      expansion
;
; Appendix I. Macro-writting macros, 'with-gensyms', to guranttee that rule c
; gets observed.
; Example usage of `with-gensyms`
(defmacro do-primes-a ((var start end) &body body)
  "do-primes implementation with macro-writting macro 'with-gensyms'"
  (with-gensyms (end-value-name)
    `(do ((,var (next-prime (- ,start 1)) (next-prime ,var))
          (,end-value-name ,end))
         ((> ,var ,end-value-name))
      ,@body)))
; Define the macro, note how comma is used to interpolate the value of the loop
; expression
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@body)
)