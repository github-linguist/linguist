(define (random-from-list list) (list-ref list (random (length list))))
(define (random-permutation list)
  (if (null? list)
      '()
      (let* ((car (random-from-list list))
             (cdr (random-permutation (remove car list))))
        (cons car cdr))))
(define (random-configuration) (random-permutation '(goat goat car)))
(define (random-door) (random-from-list '(0 1 2)))

(define (trial strategy)
  (define (door-with-goat-other-than door strategy)
    (cond ((and (not (= 0 door)) (equal? (list-ref strategy 0) 'goat)) 0)
          ((and (not (= 1 door)) (equal? (list-ref strategy 1) 'goat)) 1)
          ((and (not (= 2 door)) (equal? (list-ref strategy 2) 'goat)) 2)))
  (let* ((configuration (random-configuration))
         (players-first-guess (strategy `(would-you-please-pick-a-door?)))
         (door-to-show-player (door-with-goat-other-than players-first-guess
                                                         configuration))
         (players-final-guess (strategy `(there-is-a-goat-at/would-you-like-to-move?
                                          ,players-first-guess
                                          ,door-to-show-player))))
    (if (equal? (list-ref configuration players-final-guess) 'car)
        'you-win!
        'you-lost)))

(define (stay-strategy message)
  (case (car message)
    ((would-you-please-pick-a-door?) (random-door))
    ((there-is-a-goat-at/would-you-like-to-move?)
     (let ((first-choice (cadr message)))
        first-choice))))

(define (switch-strategy message)
  (case (car message)
    ((would-you-please-pick-a-door?) (random-door))
    ((there-is-a-goat-at/would-you-like-to-move?)
     (let ((first-choice (cadr message))
           (shown-goat (caddr message)))
       (car (remove first-choice (remove shown-goat '(0 1 2))))))))

(define-syntax repeat
  (syntax-rules ()
    ((repeat <n> <body> ...)
     (let loop ((i <n>))
       (if (zero? i)
           '()
           (cons ((lambda () <body> ...))
                 (loop (- i 1))))))))

(define (count element list)
  (if (null? list)
      0
      (if (equal? element (car list))
          (+ 1 (count element (cdr list)))
          (count element (cdr list)))))

(define (prepare-result strategy results)
  `(,strategy won with probability
              ,(exact->inexact (* 100 (/ (count 'you-win! results) (length results)))) %))

(define (compare-strategies times)
  (append
   (prepare-result 'stay-strategy (repeat times (trial stay-strategy)))
   '(and)
   (prepare-result 'switch-strategy (repeat times (trial switch-strategy)))))

;; > (compare-strategies 1000000)
;; (stay-strategy won with probability 33.3638 %
;;  and switch-strategy won with probability 66.716 %)
