#lang racket
(require math/array)
;board uses arrays directly, but maintaining an abstraction is nice
(define (board-ref b row col) (array-ref b (vector row col)))
(define (board-rows b) (vector-ref (array-shape b) 0))
(define (board-cols b) (vector-ref (array-shape b) 1))
(define (on-board? b row col)
  (and (<= 0 row (sub1 (board-rows b)))
       (<= 0 col (sub1 (board-cols b)))))
(define (board->lists b) (array->list* b))
;run on adjacent board positions
(define-syntax (for-adj stx)
  (syntax-case stx ()
    [(_ b (r row) (c col) diag? body ...)
     (with-syntax ([is (if (syntax->datum #'diag?) #''(0 0 1 1 1 -1 -1 -1) #''(0 0 1 -1))]
                   [js (if (syntax->datum #'diag?) #''(1 -1 0 -1 1 0 -1 1) #''(1 -1 0 0))])
       #'(for ([i is] [j js])
           (let ([r (+ row i)]
                 [c (+ col j)])
             (when (on-board? b r c)
               body ...))))]))
;mark is either hidden, assume-mine, or clear
;n is int equal to # adj mines or -1 for mine
(struct pos ([mark #:mutable] n) #:transparent)
(define (mine? p) (= (pos-n p) -1))
(define (mine-count b) (apply + (array->list (array-map (λ (p) (if (mine? p) 1 0)) b))))
;hidden0? is needed because only spaces with no mines in them and no mines adjacent
;to them are cleared recursively
(define (hidden0? p)
  (and (symbol=? (pos-mark p) 'hidden)
       (zero? (pos-n p))))
(define (show-pos p)
  (match-let ([(pos m n) p])
    (case m
      [(hidden) "."]
      [(assume-mine) "?"]
      [(clear) (if (zero? n) " " (number->string n))]
      [else (error "illegal mark" m)])))
;put "|" around positions
(define (show-board b)
  (for ([row (board->lists b)])
    (displayln (format "|~a|" (string-join (map show-pos row) "|")))))

;winning = every position is either cleared or a hidden mine
(define (win? b)
  (for*/and ([r (range 0 (board-rows b))]
             [c (range 0 (board-cols b))])
    (let ([p (board-ref b r c)])
      (or (symbol=? (pos-mark p) 'clear)
          (mine? p)))))

(define (init-board rows cols)
  (let ([chance (+ (/ (random) 10) 0.1)]
        ;empty board
        [b (array->mutable-array (build-array (vector rows cols)
                                              (λ (x) (pos 'hidden 0))))])
    ;loop whole board
    (for* ([row (range 0 rows)]
           [col (range 0 cols)])
      (when (< (random) chance)
        ;put a mine
        (array-set! b (vector row col) (pos 'hidden -1))
        ;increment adjacent mine counts unless that adjacent position is a mine
        (for-adj b (r row) (c col) #t
                 (let ([p (board-ref b r c)])
                   (unless (mine? p)
                     (array-set! b (vector r c) (pos 'hidden (add1 (pos-n p)))))))))
    b))

;only clear position if it's not a mine
;only continue recursing when it's a hidden0?
(define (try-clear! p)
  (cond [(mine? p) #f]
        [(hidden0? p) (set-pos-mark! p 'clear) #t]
        [else (set-pos-mark! p 'clear) #f]))

;the following player move functions return boolean where #f = lose, #t = still going
;assuming can never directly lose ((void) == #t from the set!)
;make sure to not allow overwriting an already cleared position
(define (toggle-assume! b row col)
  (let ([p (board-ref b row col)])
    (set-pos-mark! p (case (pos-mark p)
                       [(assume-mine) 'hidden]
                       [(hidden) 'assume-mine]
                       [(clear) 'clear]
                       [else (error "invalid mark" (pos-mark p))]))))

;clearing loses when the chosen position is a mine
;void = #t as far as if works, so no need to return #t
(define (clear! b row col)
  (let ([p (board-ref b row col)])
    (and (not (mine? p))
         ;not a mine, so recursively check adjacents, and maintain list of visited positions
         ;to avoid infinite loops
         (let ([seen '()])
           ;clear the chosen position first, only continuing if it's a 0
           (when (try-clear! p)
             (let clear-adj ([row row] [col col])
               (for-adj b (r row) (c col) #f
                        ;make sure its not seen
                        (when (and (not (member (list r c) seen))
                                   (try-clear! (board-ref b r c)))
                          ;it was cleared, so loop after saving this position as being seen
                          (set! seen (cons (list r c) seen))
                          (clear-adj r c)))))))))

(define assume-string "a")
(define clear-string "c")
;validates input...returns either #f for an error or the move to execute
(define (parse-and-create-move! b s)
  (match (string-split s)
    [(list type row col)
     (let ([row (string->number row)]
           [col (string->number col)])
       (and (number? row)
            (number? col)
            (let ([row (sub1 row)]
                  [col (sub1 col)])
              (and (on-board? b row col)
                   (or (and (string=? type assume-string) (λ () (toggle-assume! b row col)))
                       (and (string=? type clear-string) (λ () (clear! b row col))))))))]
    [else #f]))
(define (run)
  (displayln (string-append "--- Enter one of:\n"
                            (format "--- \"~a <row> <col>\" to clear at (row,col), or~n" clear-string)
                            (format (string-append "--- \"~a <row> <col>\" to flag a possible mine "
                                                   "(or clear a flag) at (row,col).~n")
                                    assume-string)))
  (let ([b (init-board 4 6)])
    (displayln (format "There are ~a mines.~n" (mine-count b)))
    (let run ()
      (show-board b)
      (display "enter move: ")
      ;parse either failed or gave the procedure to execute
      (let ([proc? (parse-and-create-move! b (read-line))])
        ;was the parse successful?
        (if proc?
            ;then run it
            (if (proc?)
                ;didn't lose, so either we won or we're not done
                (if (win? b) (displayln "CLEAR!") (run))
                (displayln "BOOM!"))
            ;parse failed
            (run))))))
