#lang racket
;print column of width w
(define (display-col w s)
  (let* ([n-spaces (- w (string-length s))]
         [spaces (make-string n-spaces #\space)])
    (display (string-append s spaces))))
;print columns given widths (idea borrowed from PicoLisp)
(define (tab ws . ss) (for-each display-col ws ss) (newline))

(define input "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")

(define (paren? s) (or (string=? s "(") (string=? s ")")))
(define-values (prec lasso? rasso? op?)
  (let ([table '(["^" 4 r]
                 ["*" 3 l]
                 ["/" 3 l]
                 ["+" 2 l]
                 ["-" 2 l])])
    (define (asso x) (caddr (assoc x table)))
    (values (λ (x) (cadr (assoc x table)))
            (λ (x) (symbol=? (asso x) 'l))
            (λ (x) (symbol=? (asso x) 'r))
            (λ (x) (member x (map car table))))))

(define (shunt s)
  (define widths (list 8 (string-length input) (string-length input) 20))
  (tab widths "TOKEN" "OUT" "STACK" "ACTION")
  (let shunt ([out '()] [ops '()] [in (string-split s)] [action ""])
    (match in
      ['() (if (memf paren? ops)
               (error "unmatched parens")
               (reverse (append (reverse ops) out)))]
      [(cons x in)
       (tab widths x (string-join (reverse out) " ") (string-append* ops) action)
       (match x
         [(? string->number n) (shunt (cons n out) ops in (format "out ~a" n))]
         ["(" (shunt out (cons "(" ops) in "push (")]
         [")" (let-values ([(l r) (splitf-at ops (λ (y) (not (string=? y "("))))])
                (match r
                  ['() (error "unmatched parens")]
                  [(cons _ r) (shunt (append (reverse l) out) r in "clear til )")]))]
         [else (let-values ([(l r) (splitf-at ops (λ (y) (and (op? y)
                                                              ((if (lasso? x) <= <) (prec x) (prec y)))))])
                 (shunt (append (reverse l) out) (cons x r) in (format "out ~a, push ~a" l x)))])])))
