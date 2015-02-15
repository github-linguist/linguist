#lang racket
(require data/heap
  "../lib/vector2.rkt" "../lib/queue.rkt" (only-in "../lib/util.rkt" push! tstruct ret awhen))

(define level (list "#######"
                    "#     #"
                    "#     #"
                    "#. #  #"
                    "#. $$ #"
                    "#.$$  #"
                    "#.#  @#"
                    "#######"))
(define (strings->vec2 l) (lists->vec2 (map string->list l)))
;turn everything except walls into distance from goals
(define (clear-level l)
  (ret ([l (vec2-copy l)])
    (define dots (vec2-atsq l #\.))
    (define q (list->q (map (λ (p) (cons p 0)) dots)))
    (let bfs () ;this search has implicit history in the mutated vector2
      (unless (nilq? q)
        (match-define (cons p n) (deq! q))
        (define x (vec2@ l p))
        ;stop if position is either a wall or a previously filled number
        (cond [(or (eq? x #\#) (number? x)) (bfs)]
              [else (vec2! l p n)
                    (for-adj l x [p p] #f (enq! (cons p (add1 n)) q))
                    (bfs)])))))

;corresponds to PicoLisp's move table in "moves", while also adding a push direction mapping
(tstruct move (f d))
(define-values (mu md ml mr LURD)
  (let ()
    (define t (map (λ (x) (cons (car x) (apply pos (cdr x))))
                   '([#\u -1 0] [#\d 1 0] [#\l 0 -1] [#\r 0 1])))
    (define (mv d)
      (define x (assoc d t))
      (move (λ (p) (pos+ p (cdr x))) (car x)))
    (values (mv #\u) (mv #\d) (mv #\l) (mv #\r)
            (λ (d) (char-upcase (car (findf (λ (x) (equal? d (cdr x))) t)))))))

;state = player pos * box poses
(tstruct st (p b))
(define (st= s1 s2) (andmap (λ (b) (member b (st-b s2))) (st-b s1)))
(define (box? p s) (member p (st-b s)))
;calculates value of a state for insertion into priority queue
;value is sum of box distances from goals
(define (value s l) (apply + (map (λ (p) (vec2@ l p)) (st-b s))))
;init state for a level
(define (st0 l) (st (vec2-atq l #\@) (vec2-atsq l #\$)))
(define (make-solution-checker l)
  (define dots (vec2-atsq l #\.))
  (λ (s) (andmap (λ (b) (member b dots)) (st-b s))))

;state after push * lurd history
(tstruct push (st h))
(define (pushes s l)
  (ret ([pushes '()])
    (for ([b (in-list (st-b s))])
      (for-adj l a [p b] #f
        (define d (pos- p b)) ;direction of push
        (define op (pos- b d)) ;where player stands to push
        (define o (vec2@ l op))
        ;make sure push pos and push dest are clear
        (when (and (number? a) (number? o)
                   (not (box? p s)) (not (box? op s)))
          (awhen [@ (moves s op l)]
            (define new-st (st b (cons p (remove b (st-b s)))))
            (push! (push new-st (cons (LURD d) @)) pushes)))))))

;state * goal pos * level -> lurd string
(define (moves s g l)
  (define h '())
  (define q (list->q (list (list (st-p s)))))
  (let bfs ()
    (if (nilq? q)
        #f
        (match-let ([(cons p lurd) (deq! q)])
          (cond [(equal? p g) lurd]
                [(or (char=? (vec2@ l p) #\#) (box? p s) (member p h)) (bfs)]
                [else (push! p h)
                      (for-each (λ (m)
                                  (match-define (move f s) m)
                                  (enq! (cons (f p) (cons s lurd)) q))
                                (list mu md ml mr))
                      (bfs)])))))

(define (sokoban l)
  (define-values (clear s0 solved?)
    (let ([l (strings->vec2 l)])
      (values (clear-level l) (st0 l) (make-solution-checker l))))
  (define h '())
  (tstruct q-elem (s lurd v)) ;priority queue stores state, lurd hist, and value
  (define (elem<= s1 s2) (<= (q-elem-v s1) (q-elem-v s2))) ;compare wrapped values
  ;queue stores a single element at the beginning consisting of:
  ;1. starting state, 2. empty lurd history, 3. value of starting state
  (define q (vector->heap elem<= (vector (q-elem s0 '() (value s0 clear)))))
  (let bfs ()
    (match-define (q-elem s lurd _) (heap-min q))
    (heap-remove-min! q)
    (cond [(solved? s) (list->string (reverse lurd))]
          [(memf (λ (s1) (st= s s1)) h) (bfs)]
          [else (push! s h)
                (for-each (λ (p)
                            (define s (push-st p))
                            (heap-add! q (q-elem s (append (push-h p) lurd) (value s clear))))
                          (pushes s clear))
                (bfs)])))
