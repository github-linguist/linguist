#lang racket

;; Utility interfaces to the low-level command
(define (capability? cap) (system (~a "tput "cap" > /dev/null 2>&1")))
(define (tput . xs) (system (apply ~a 'tput " " (add-between xs " "))) (void))
(define (colorterm?) (and (capability? 'setaf) (capability? 'setab)))
(define color-map '([black 0] [red 1] [green 2] [yellow 3]
                    [blue 4] [magenta 5] [cyan 6] [white 7]))
(define (foreground color) (tput 'setaf (cadr (assq color color-map))))
(define (background color) (tput 'setab (cadr (assq color color-map))))
(define (reset) (tput 'sgr0) (void))

;; Demonstration of use
(if (colorterm?)
  (begin (foreground 'blue)
         (background 'yellow)
         (displayln "Color output")
         (reset))
  (displayln "Monochrome only"))
(if (capability? 'blink)
  (begin (tput 'blink)
         (displayln "Blinking output")
         (reset))
  (displayln "Steady only"))
