#lang racket
(require racket/control)

(define (make-fringe-getter tree)
  (λ ()
    (let loop ([tree tree])
      (match tree
        [(cons a d) (loop a)
                    (loop d)]
        ['()        (void)]
        [else       (fcontrol tree)]))
    (fcontrol 'done)))

(define (same-fringe? tree1 tree2)
  (let loop ([get-fringe1 (make-fringe-getter tree1)]
             [get-fringe2 (make-fringe-getter tree2)])
    (% (get-fringe1)
       (λ (fringe1 get-fringe1)
         (% (get-fringe2)
            (λ (fringe2 get-fringe2)
              (and (equal? fringe1 fringe2)
                   (or (eq? fringe1 'done)
                       (loop get-fringe1 get-fringe2)))))))))

;; unit tests
(require rackunit)
(check-true (same-fringe? '((1 2 3) ((4 5 6) (7 8)))
                          '(((1 2 3) (4 5 6)) (7 8))))
(check-false (same-fringe? '((1 2 3) ((4 5 6) (7 8)))
                           '(((1 2 3) (4 6)) (8))))
