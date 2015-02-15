#lang racket
(require net/url html xml xml/path)

(struct item (name mass value count) #:transparent)

;this section is to convert the web page on the problem into the data for the problem
;i don't got time to manually type tables, nevermind that this took longer
(define (group-n n l)
  (let group-n ([l l] [acc '()])
    (if (null? l)
        (reverse acc)
        (let-values ([(takes drops) (split-at l n)])
          (group-n drops (cons takes acc))))))
(define (atom? x) (not (or (pair? x) (null? x))))
;modified from little schemer...finds nested list where regular member would've returned non-#f
(define (member* x t)
  (cond [(null? t) #f]
        [(atom? (car t)) (or (and (equal? (car t) x) t)
                             (member* x (cdr t)))]
        [else (or (member* x (car t))
                  (member* x (cdr t)))]))
(define (addr->xexpr f) (compose xml->xexpr f read-html-as-xml get-pure-port string->url))
(define (read-page) ((addr->xexpr cadr) "http://rosettacode.org/wiki/Knapsack_problem/Bounded"))
(define (get-xml-table xe) (member* 'table xe))
(define (xml-table->item-list xe-table)
  ;all html table datas
  (let* ([strs (se-path*/list '(td) xe-table)]
         ;4 columns per row
         [rows (group-n 4 strs)]
         ;the last two rows belong to the knapsack entry which we don't want
         [rows (take rows (- (length rows) 2))])
    (for/list ([r rows])
      (let ([r (map string-trim r)])
        ;convert the weight, value, and pieces columns to numbers and structify it
        (apply item (car r) (map string->number (cdr r)))))))
;top-level function to take a string representing a URL and gives the table I didn't want to type
(define addr->item-list (compose xml-table->item-list get-xml-table read-page))

;stores best solution
(struct solution (value choices) #:transparent)

;finds best solution in a list of solutions
(define (best sol-list)
  (let best ([l (cdr sol-list)] [bst (car sol-list)])
    (match l
      ['() bst]
      [(cons s l) (if (> (solution-value s) (solution-value bst))
                      (best l s)
                      (best l bst))])))

;stores the choices leading to best solution...item name and # of that item taken
(struct choice (name count) #:transparent)

;algorithm is derived from Haskell's array-based example
;returns vector of solutions for every natural number capacity up to the input max
(define (solution-vector capacity)
  ;find best value and items that gave it, trying all items
  ;first we set up a vector for the best solution found for every capacity
  ;the best solution found at the beginning has 0 value with no items
  (for/fold ([solutions (make-vector (add1 capacity) (solution 0 '()))])
            ([i (addr->item-list)])
    ;the new solutions aren't accumulated until after processing all the way through a particular
    ;capacity, lest capacity c allow capacity c+1 to reuse an item that had been exhausted, i.e.
    ;we have to move on to the next item before we save ANY solutions found for that item, so they
    ;must be saved "in parallel" by overwriting the entire vector at once
    (for/vector ([target-cap (range 0 (add1 capacity))])
      (match-let ([(item name mass value count) i])
        ;find best solution for item out of every number that can be taken of that item
        ;we'll call an "item and count of item" a choice
        (best (for/list ([n (range 0 (add1 count))])
                ;ensure mass of this choice is not greater than target capacity
                (let ([mass-choice (* n mass)])
                  (if (> mass-choice target-cap)
                      (solution 0 '())
                      ;subtract from target capacity the amount taken up by this choice
                      ;use it to get the best solution for THAT capacity
                      (let ([remaining-cap-solution (vector-ref solutions (- target-cap mass-choice))])
                        ;the new solution found adds the value of this choice to the above value and
                        ;adds the choice itself to the list of choices
                        (solution (+ (* n value) (solution-value remaining-cap-solution))
                                  (cons (choice name n) (solution-choices remaining-cap-solution))))))))))))

;top level function to convert solution vector into single best answer
(define (knapsack capacity)
  ;the best solution is not necessarily the one that uses max capacity,
  ;e.g. if max is 5 and you have items of mass 4 and 5, and the 4 is worth more
  (match-let ([(solution value choices) (best (vector->list (solution-vector capacity)))])
    (solution value (filter (compose positive? choice-count) choices))))
