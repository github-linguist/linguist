#lang racket

(require plot)

(define (dragon-turn n)
  (if (> (bitwise-and (arithmetic-shift (bitwise-and n (- n)) 1) n) 0)
      'L
      'R))

(define (rotate heading dir)
  (cond
    [(eq? dir 'R) (cond [(eq? heading 'N) 'E]
                        [(eq? heading 'E) 'S]
                        [(eq? heading 'S) 'W]
                        [(eq? heading 'W) 'N])]
    [(eq? dir 'L) (cond [(eq? heading 'N) 'W]
                        [(eq? heading 'E) 'N]
                        [(eq? heading 'S) 'E]
                        [(eq? heading 'W) 'S])]))
(define (step pos heading)
  (cond
    [(eq? heading 'N) (list (car pos) (add1 (cadr pos)))]
    [(eq? heading 'E) (list (add1 (car pos)) (cadr pos))]
    [(eq? heading 'S) (list (car pos) (sub1 (cadr pos)))]
    [(eq? heading 'W) (list (sub1 (car pos)) (cadr pos))]
    ))

(let-values ([(dir pos trail)
              (for/fold ([dir 'N]
                         [pos (list 0 0)]
                         [trail '((0 0))])
                ([n (in-range 0 50000)])
                (let* ([new-dir (rotate dir (dragon-turn n))]
                       [new-pos (step pos new-dir)])
                  (values new-dir
                          new-pos
                          (cons new-pos trail))))])
  (plot-file (lines trail) "dragon.png" 'png))
