#lang racket

(define sema (make-semaphore 4)) ; allow 4 concurrent jobs

;; start 20 jobs and wait for all of them to end
(for-each
 thread-wait
 (for/list ([i 20])
   (thread (λ() (semaphore-wait sema)
                (printf "Job #~a acquired semaphore\n" i)
                (sleep 2)
                (printf "Job #~a done\n" i)
                (semaphore-post sema)))))
