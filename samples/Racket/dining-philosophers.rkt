#lang racket

;; Racket has traditional semaphores in addition to several higher level
;; synchronization tools.  (Note that these semaphores are used for Racket's
;; green-threads, there are also "future semaphores" which are used for OS
;; threads, with a similar interface.)

;; ----------------------------------------------------------------------------
;; First, a bunch of code to run the experiments below

;; Only two philosophers to make it deadlock very fast
(define philosophers '(Aristotle Kant #|Spinoza Marx Russell|#))

(define (run-philosopher name fork1 fork2)
  (define (show what) (displayln (~a name " " what)))
  (define (loop)
    (show "thinks") (sleep (* 2 (random))) (show "is hungry")
    (grab-forks fork1 fork2 (λ() (show "eats") (sleep (random))))
    (loop))
  (thread loop))

(define (run:simple)
  (define forks (for/list ([i philosophers]) (make-semaphore 1)))
  (for ([i philosophers] [fork1 forks] [fork2 (cons (last forks) forks)])
    (run-philosopher i fork1 fork2))
  (sleep (* 60 60 24 365)))

;; ----------------------------------------------------------------------------
;; This is the naive implementation, which can be used to try getting a
;; deadlock.

(define (grab:naive fork1 fork2 eat!)
  (semaphore-wait fork1)
  (sleep (random)) ; to make deadlocks probable
  (semaphore-wait fork2)
  (eat!)
  (semaphore-post fork1)
  (semaphore-post fork2))

;; ----------------------------------------------------------------------------
;; One way to solve it is to release the first fork if the second is busy and
;; wait for a while.

(define (grab:release+wait fork1 fork2 eat!)
  (semaphore-wait fork1)
  (if (not (semaphore-try-wait? fork2))
    ;; couldn't grab the second fork, so release the first and wait
    (begin (semaphore-post fork1)
           (sleep (random))
           (grab-forks fork1 fork2)) ; can swap them to improve chances
    ;; we have both forks
    (begin (eat!)
           (semaphore-post fork1)
           (semaphore-post fork2))))

;; ----------------------------------------------------------------------------
;; Another solution is to label the forks and lock the lowest-id one first,
;; which makes the naive solution work.

(define (run:labeled-forks)
  (define forks (for/list ([i philosophers]) (make-semaphore 1)))
  ;; the simple run used forks as (1 2 3 4) (4 1 2 3) -- so to implement this,
  ;; we can swap the two first ones: (4 2 3 4) (1 1 2 3)
  (for ([i philosophers]
        [fork1 (cons (last forks) (cdr forks))]
        [fork2 (cons (first forks) forks)])
    (run-philosopher i fork1 fork2))
  (sleep (* 60 60 24 365)))

;; ----------------------------------------------------------------------------
;; Homework: implement the centralized waiter solution

;; ...

;; ----------------------------------------------------------------------------
;; Uncomment one of the following pairs to try it

;; (define grab-forks grab:naive)
;; (define run run:simple)

;; (define grab-forks grab:release+wait)
;; (define run run:simple)

;; (define grab-forks grab:naive)
;; (define run run:labeled-forks)

(run)
