#lang racket
(require math/array)

;#f = not a legal position, #t = blank position
(define board
  (array
   #[#[#t 33 35 #t #t #f #f #f]
     #[#t #t 24 22 #t #f #f #f]
     #[#t #t #t 21 #t #t #f #f]
     #[#t 26 #t 13 40 11 #f #f]
     #[27 #t #t #t  9 #t  1 #f]
     #[#f #f #t #t 18 #t #t #f]
     #[#f #f #f #f #t  7 #t #t]
     #[#f #f #f #f #f #f  5 #t]]))

;filters elements with the predicate, returning the element and its indices
(define (array-indices-of a f)
  (for*/list ([i (range 0 (vector-ref (array-shape a) 0))]
              [j (range 0 (vector-ref (array-shape a) 1))]
              #:when (f (array-ref a (vector i j))))
    (list (array-ref a (vector i j)) i j)))

;returns a list, each element is a list of the number followed by i and j indices
;sorted ascending by number
(define (goal-list v) (sort (array-indices-of v number?) (λ (a b) (< (car a) (car b)))))

;every direction + start position that's on the board
(define (legal-moves a i0 j0)
  (for*/list ([i (range (sub1 i0) (+ i0 2))]
              [j (range (sub1 j0) (+ j0 2))]
              ;cartesian product -1..1 and -1..1, except 0 0
              #:when (and (not (and (= i i0) (= j j0)))
                          ;make sure it's on the board
                          (<= 0 i (sub1 (vector-ref (array-shape a) 0)))
                          (<= 0 j (sub1 (vector-ref (array-shape a) 1)))
                          ;make sure it's an actual position too (the real board isn't square)
                          (array-ref a (vector i j))))
    (cons i j)))

;find path through array, returning list of coords from start to finish
(define (hidato-path a)
  ;get starting position as first goal
  (match-let ([(cons (list n i j) goals) (goal-list a)])
    (let hidato ([goals goals] [n n] [i i] [j j] [path '()])
      (match goals
        ;no more goals, return path
        ['() (reverse (cons (cons i j) path))]
        ;get next goal
        [(cons (list n-goal i-goal j-goal) _)
         (let ([move (cons i j)])
           ;already visiting a spot or taking too many moves to reach the next goal is no good
           (cond [(or (member move path) (> n n-goal)) #f]
                 ;taking the right number of moves to be at the goal square is good
                 ;so go to the next goal
                 [(and (= n n-goal) (= i i-goal) (= j j-goal))
                  (hidato (cdr goals) n i j path)]
                 ;depth first search using every legal move to find next goal
                 [else (ormap (λ (m) (hidato goals (add1 n) (car m) (cdr m) (cons move path)))
                              (legal-moves a i j))]))]))))

;take a path and insert it into the array
(define (put-path a path)
  (let ([a (array->mutable-array a)])
    (for ([n (range 1 (add1 (length path)))] [move path])
      (array-set! a (vector (car move) (cdr move)) n))
    a))

;main function
(define (hidato board) (put-path board (hidato-path board)))
