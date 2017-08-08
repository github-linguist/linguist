;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         sboyer.sch
; Description:  The Boyer benchmark
; Author:       Bob Boyer
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:52:20 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               2-Jul-88 (Will Clinger -- distinguished #f and the empty list)
;               13-Feb-97 (Will Clinger -- fixed bugs in unifier and rules,
;                          rewrote to eliminate property lists, and added
;                          a scaling parameter suggested by Bob Boyer)
;               19-Mar-99 (Will Clinger -- cleaned up comments)
;               4-Apr-01 (Will Clinger -- changed four 1- symbols to sub1)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SBOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Much less CONS-intensive than NBOYER because it uses Henry Baker's
;;; "sharing cons".

; Note:  The version of this benchmark that appears in Dick Gabriel's book
; contained several bugs that are corrected here.  These bugs are discussed
; by Henry Baker, "The Boyer Benchmark Meets Linear Logic", ACM SIGPLAN Lisp
; Pointers 6(4), October-December 1993, pages 3-10.  The fixed bugs are:
;
;    The benchmark now returns a boolean result.
;    FALSEP and TRUEP use TERM-MEMBER? rather than MEMV (which is called MEMBER
;         in Common Lisp)
;    ONE-WAY-UNIFY1 now treats numbers correctly
;    ONE-WAY-UNIFY1-LST now treats empty lists correctly
;    Rule 19 has been corrected (this rule was not touched by the original
;         benchmark, but is used by this version)
;    Rules 84 and 101 have been corrected (but these rules are never touched
;         by the benchmark)
;
; According to Baker, these bug fixes make the benchmark 10-25% slower.
; Please do not compare the timings from this benchmark against those of
; the original benchmark.
;
; This version of the benchmark also prints the number of rewrites as a sanity
; check, because it is too easy for a buggy version to return the correct
; boolean result.  The correct number of rewrites is
;
;     n      rewrites       peak live storage (approximate, in bytes)
;     0         95024
;     1        591777
;     2       1813975
;     3       5375678
;     4      16445406
;     5      51507739

; Sboyer is a 2-phase benchmark.
; The first phase attaches lemmas to symbols.  This phase is not timed,
; but it accounts for very little of the runtime anyway.
; The second phase creates the test problem, and tests to see
; whether it is implied by the lemmas.

(define (sboyer-benchmark . args)
  (let ((n (if (null? args) 0 (car args))))
    (setup-boyer)
    (run-benchmark (string-append "sboyer"
                                  (number->string n))
                   1
                   (lambda () (test-boyer n))
                   (lambda (rewrites)
                     (and (number? rewrites)
                          (case n
                           ((0)  (= rewrites 95024))
                           ((1)  (= rewrites 591777))
                           ((2)  (= rewrites 1813975))
                           ((3)  (= rewrites 5375678))
                           ((4)  (= rewrites 16445406))
                           ((5)  (= rewrites 51507739))
                           ; If it works for n <= 5, assume it works.
                           (else #t)))))))

(define (setup-boyer) #t) ; assigned below
(define (test-boyer) #t)  ; assigned below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The first phase.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the original benchmark, it stored a list of lemmas on the
; property lists of symbols.
; In the new benchmark, it maintains an association list of
; symbols and symbol-records, and stores the list of lemmas
; within the symbol-records.

(let ()
  
  (define (setup)
    (add-lemma-lst
     (quote ((equal (compile form)
                    (reverse (codegen (optimize form)
                                      (nil))))
             (equal (eqp x y)
                    (equal (fix x)
                           (fix y)))
             (equal (greaterp x y)
                    (lessp y x))
             (equal (lesseqp x y)
                    (not (lessp y x)))
             (equal (greatereqp x y)
                    (not (lessp x y)))
             (equal (boolean x)
                    (or (equal x (t))
                        (equal x (f))))
             (equal (iff x y)
                    (and (implies x y)
                         (implies y x)))
             (equal (even1 x)
                    (if (zerop x)
                        (t)
                        (odd (sub1 x))))
             (equal (countps- l pred)
                    (countps-loop l pred (zero)))
             (equal (fact- i)
                    (fact-loop i 1))
             (equal (reverse- x)
                    (reverse-loop x (nil)))
             (equal (divides x y)
                    (zerop (remainder y x)))
             (equal (assume-true var alist)
                    (cons (cons var (t))
                          alist))
             (equal (assume-false var alist)
                    (cons (cons var (f))
                          alist))
             (equal (tautology-checker x)
                    (tautologyp (normalize x)
                                (nil)))
             (equal (falsify x)
                    (falsify1 (normalize x)
                              (nil)))
             (equal (prime x)
                    (and (not (zerop x))
                         (not (equal x (add1 (zero))))
                         (prime1 x (sub1 x))))
             (equal (and p q)
                    (if p (if q (t)
                                (f))
                          (f)))
             (equal (or p q)
                    (if p (t)
                          (if q (t)
                                (f))))
             (equal (not p)
                    (if p (f)
                          (t)))
             (equal (implies p q)
                    (if p (if q (t)
                                (f))
                          (t)))
             (equal (fix x)
                    (if (numberp x)
                        x
                        (zero)))
             (equal (if (if a b c)
                        d e)
                    (if a (if b d e)
                          (if c d e)))
             (equal (zerop x)
                    (or (equal x (zero))
                        (not (numberp x))))
             (equal (plus (plus x y)
                          z)
                    (plus x (plus y z)))
             (equal (equal (plus a b)
                           (zero))
                    (and (zerop a)
                         (zerop b)))
             (equal (difference x x)
                    (zero))
             (equal (equal (plus a b)
                           (plus a c))
                    (equal (fix b)
                           (fix c)))
             (equal (equal (zero)
                           (difference x y))
                    (not (lessp y x)))
             (equal (equal x (difference x y))
                    (and (numberp x)
                         (or (equal x (zero))
                             (zerop y))))
             (equal (meaning (plus-tree (append x y))
                             a)
                    (plus (meaning (plus-tree x)
                                   a)
                          (meaning (plus-tree y)
                                   a)))
             (equal (meaning (plus-tree (plus-fringe x))
                             a)
                    (fix (meaning x a)))
             (equal (append (append x y)
                            z)
                    (append x (append y z)))
             (equal (reverse (append a b))
                    (append (reverse b)
                            (reverse a)))
             (equal (times x (plus y z))
                    (plus (times x y)
                          (times x z)))
             (equal (times (times x y)
                           z)
                    (times x (times y z)))
             (equal (equal (times x y)
                           (zero))
                    (or (zerop x)
                        (zerop y)))
             (equal (exec (append x y)
                          pds envrn)
                    (exec y (exec x pds envrn)
                            envrn))
             (equal (mc-flatten x y)
                    (append (flatten x)
                            y))
             (equal (member x (append a b))
                    (or (member x a)
                        (member x b)))
             (equal (member x (reverse y))
                    (member x y))
             (equal (length (reverse x))
                    (length x))
             (equal (member a (intersect b c))
                    (and (member a b)
                         (member a c)))
             (equal (nth (zero)
                         i)
                    (zero))
             (equal (exp i (plus j k))
                    (times (exp i j)
                           (exp i k)))
             (equal (exp i (times j k))
                    (exp (exp i j)
                         k))
             (equal (reverse-loop x y)
                    (append (reverse x)
                            y))
             (equal (reverse-loop x (nil))
                    (reverse x))
             (equal (count-list z (sort-lp x y))
                    (plus (count-list z x)
                          (count-list z y)))
             (equal (equal (append a b)
                           (append a c))
                    (equal b c))
             (equal (plus (remainder x y)
                          (times y (quotient x y)))
                    (fix x))
             (equal (power-eval (big-plus1 l i base)
                                base)
                    (plus (power-eval l base)
                          i))
             (equal (power-eval (big-plus x y i base)
                                base)
                    (plus i (plus (power-eval x base)
                                  (power-eval y base))))
             (equal (remainder y 1)
                    (zero))
             (equal (lessp (remainder x y)
                           y)
                    (not (zerop y)))
             (equal (remainder x x)
                    (zero))
             (equal (lessp (quotient i j)
                           i)
                    (and (not (zerop i))
                         (or (zerop j)
                             (not (equal j 1)))))
             (equal (lessp (remainder x y)
                           x)
                    (and (not (zerop y))
                         (not (zerop x))
                         (not (lessp x y))))
             (equal (power-eval (power-rep i base)
                                base)
                    (fix i))
             (equal (power-eval (big-plus (power-rep i base)
                                          (power-rep j base)
                                          (zero)
                                          base)
                                base)
                    (plus i j))
             (equal (gcd x y)
                    (gcd y x))
             (equal (nth (append a b)
                         i)
                    (append (nth a i)
                            (nth b (difference i (length a)))))
             (equal (difference (plus x y)
                                x)
                    (fix y))
             (equal (difference (plus y x)
                                x)
                    (fix y))
             (equal (difference (plus x y)
                                (plus x z))
                    (difference y z))
             (equal (times x (difference c w))
                    (difference (times c x)
                                (times w x)))
             (equal (remainder (times x z)
                               z)
                    (zero))
             (equal (difference (plus b (plus a c))
                                a)
                    (plus b c))
             (equal (difference (add1 (plus y z))
                                z)
                    (add1 y))
             (equal (lessp (plus x y)
                           (plus x z))
                    (lessp y z))
             (equal (lessp (times x z)
                           (times y z))
                    (and (not (zerop z))
                         (lessp x y)))
             (equal (lessp y (plus x y))
                    (not (zerop x)))
             (equal (gcd (times x z)
                         (times y z))
                    (times z (gcd x y)))
             (equal (value (normalize x)
                           a)
                    (value x a))
             (equal (equal (flatten x)
                           (cons y (nil)))
                    (and (nlistp x)
                         (equal x y)))
             (equal (listp (gopher x))
                    (listp x))
             (equal (samefringe x y)
                    (equal (flatten x)
                           (flatten y)))
             (equal (equal (greatest-factor x y)
                           (zero))
                    (and (or (zerop y)
                             (equal y 1))
                         (equal x (zero))))
             (equal (equal (greatest-factor x y)
                           1)
                    (equal x 1))
             (equal (numberp (greatest-factor x y))
                    (not (and (or (zerop y)
                                  (equal y 1))
                              (not (numberp x)))))
             (equal (times-list (append x y))
                    (times (times-list x)
                           (times-list y)))
             (equal (prime-list (append x y))
                    (and (prime-list x)
                         (prime-list y)))
             (equal (equal z (times w z))
                    (and (numberp z)
                         (or (equal z (zero))
                             (equal w 1))))
             (equal (greatereqp x y)
                    (not (lessp x y)))
             (equal (equal x (times x y))
                    (or (equal x (zero))
                        (and (numberp x)
                             (equal y 1))))
             (equal (remainder (times y x)
                               y)
                    (zero))
             (equal (equal (times a b)
                           1)
                    (and (not (equal a (zero)))
                         (not (equal b (zero)))
                         (numberp a)
                         (numberp b)
                         (equal (sub1 a)
                                (zero))
                         (equal (sub1 b)
                                (zero))))
             (equal (lessp (length (delete x l))
                           (length l))
                    (member x l))
             (equal (sort2 (delete x l))
                    (delete x (sort2 l)))
             (equal (dsort x)
                    (sort2 x))
             (equal (length (cons x1
                                  (cons x2
                                        (cons x3 (cons x4
                                                       (cons x5
                                                             (cons x6 x7)))))))
                    (plus 6 (length x7)))
             (equal (difference (add1 (add1 x))
                                2)
                    (fix x))
             (equal (quotient (plus x (plus x y))
                              2)
                    (plus x (quotient y 2)))
             (equal (sigma (zero)
                           i)
                    (quotient (times i (add1 i))
                              2))
             (equal (plus x (add1 y))
                    (if (numberp y)
                        (add1 (plus x y))
                        (add1 x)))
             (equal (equal (difference x y)
                           (difference z y))
                    (if (lessp x y)
                        (not (lessp y z))
                        (if (lessp z y)
                            (not (lessp y x))
                            (equal (fix x)
                                   (fix z)))))
             (equal (meaning (plus-tree (delete x y))
                             a)
                    (if (member x y)
                        (difference (meaning (plus-tree y)
                                             a)
                                    (meaning x a))
                        (meaning (plus-tree y)
                                 a)))
             (equal (times x (add1 y))
                    (if (numberp y)
                        (plus x (times x y))
                        (fix x)))
             (equal (nth (nil)
                         i)
                    (if (zerop i)
                        (nil)
                        (zero)))
             (equal (last (append a b))
                    (if (listp b)
                        (last b)
                        (if (listp a)
                            (cons (car (last a))
                                  b)
                            b)))
             (equal (equal (lessp x y)
                           z)
                    (if (lessp x y)
                        (equal (t) z)
                        (equal (f) z)))
             (equal (assignment x (append a b))
                    (if (assignedp x a)
                        (assignment x a)
                        (assignment x b)))
             (equal (car (gopher x))
                    (if (listp x)
                        (car (flatten x))
                        (zero)))
             (equal (flatten (cdr (gopher x)))
                    (if (listp x)
                        (cdr (flatten x))
                        (cons (zero)
                              (nil))))
             (equal (quotient (times y x)
                              y)
                    (if (zerop y)
                        (zero)
                        (fix x)))
             (equal (get j (set i val mem))
                    (if (eqp j i)
                        val
                        (get j mem)))))))
  
  (define (add-lemma-lst lst)
    (cond ((null? lst)
           #t)
          (else (add-lemma (car lst))
                (add-lemma-lst (cdr lst)))))
  
  (define (add-lemma term)
    (cond ((and (pair? term)
                (eq? (car term)
                     (quote equal))
                (pair? (cadr term)))
           (put (car (cadr term))
                (quote lemmas)
                (cons
                 (translate-term term)
                 (get (car (cadr term)) (quote lemmas)))))
          (else (error "ADD-LEMMA did not like term:  " term))))
  
  ; Translates a term by replacing its constructor symbols by symbol-records.
  
  (define (translate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (symbol->symbol-record (car term))
                      (translate-args (cdr term))))))
  
  (define (translate-args lst)
    (cond ((null? lst)
           '())
          (else (cons (translate-term (car lst))
                      (translate-args (cdr lst))))))
  
  ; For debugging only, so the use of MAP does not change
  ; the first-order character of the benchmark.
  
  (define (untranslate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (get-name (car term))
                      (map untranslate-term (cdr term))))))
  
  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.
  
  (define (put sym property value)
    (put-lemmas! (symbol->symbol-record sym) value))
  
  (define (get sym property)
    (get-lemmas (symbol->symbol-record sym)))
  
  (define (symbol->symbol-record sym)
    (let ((x (assq sym *symbol-records-alist*)))
      (if x
          (cdr x)
          (let ((r (make-symbol-record sym)))
            (set! *symbol-records-alist*
                  (cons (cons sym r)
                        *symbol-records-alist*))
            r))))
  
  ; Association list of symbols and symbol-records.
  
  (define *symbol-records-alist* '())
  
  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.
  
  (define (make-symbol-record sym)
    (vector sym '()))
  
  (define (put-lemmas! symbol-record lemmas)
    (vector-set! symbol-record 1 lemmas))
  
  (define (get-lemmas symbol-record)
    (vector-ref symbol-record 1))
  
  (define (get-name symbol-record)
    (vector-ref symbol-record 0))
  
  (define (symbol-record-equal? r1 r2)
    (eq? r1 r2))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; The second phase.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (test n)
    (let ((term
           (apply-subst
            (translate-alist
             (quote ((x f (plus (plus a b)
                                (plus c (zero))))
                     (y f (times (times a b)
                                 (plus c d)))
                     (z f (reverse (append (append a b)
                                           (nil))))
                     (u equal (plus a b)
                              (difference x y))
                     (w lessp (remainder a b)
                              (member a (length b))))))
            (translate-term
             (do ((term
                   (quote (implies (and (implies x y)
                                        (and (implies y z)
                                             (and (implies z u)
                                                  (implies u w))))
                                   (implies x w)))
                   (list 'or term '(f)))
                  (n n (- n 1)))
                 ((zero? n) term))))))
    (tautp term)))
  
  (define (translate-alist alist)
    (cond ((null? alist)
           '())
          (else (cons (cons (caar alist)
                            (translate-term (cdar alist)))
                      (translate-alist (cdr alist))))))
  
  (define (apply-subst alist term)
    (cond ((not (pair? term))
           (let ((temp-temp (assq term alist)))
             (if temp-temp
                 (cdr temp-temp)
                 term)))
          (else (cons (car term)
                      (apply-subst-lst alist (cdr term))))))
  
  (define (apply-subst-lst alist lst)
    (cond ((null? lst)
           '())
          (else (cons (apply-subst alist (car lst))
                      (apply-subst-lst alist (cdr lst))))))
  
  (define (tautp x)
    (tautologyp (rewrite x)
                '() '()))
  
  (define (tautologyp x true-lst false-lst)
    (cond ((truep x true-lst)
           #t)
          ((falsep x false-lst)
           #f)
          ((not (pair? x))
           #f)
          ((eq? (car x) if-constructor)
           (cond ((truep (cadr x)
                         true-lst)
                  (tautologyp (caddr x)
                              true-lst false-lst))
                 ((falsep (cadr x)
                          false-lst)
                  (tautologyp (cadddr x)
                              true-lst false-lst))
                 (else (and (tautologyp (caddr x)
                                        (cons (cadr x)
                                              true-lst)
                                        false-lst)
                            (tautologyp (cadddr x)
                                        true-lst
                                        (cons (cadr x)
                                              false-lst))))))
          (else #f)))
  
  (define if-constructor '*) ; becomes (symbol->symbol-record 'if)
  
  (define rewrite-count 0) ; sanity check
  
  ; The next procedure is Henry Baker's sharing CONS, which avoids
  ; allocation if the result is already in hand.
  ; The REWRITE and REWRITE-ARGS procedures have been modified to
  ; use SCONS instead of CONS.
  
  (define (scons x y original)
    (if (and (eq? x (car original))
             (eq? y (cdr original)))
        original
        (cons x y)))
  
  (define (rewrite term)
    (set! rewrite-count (+ rewrite-count 1))
    (cond ((not (pair? term))
           term)
          (else (rewrite-with-lemmas (scons (car term)
                                            (rewrite-args (cdr term))
                                            term)
                                     (get-lemmas (car term))))))
  
  (define (rewrite-args lst)
    (cond ((null? lst)
           '())
          (else (scons (rewrite (car lst))
                       (rewrite-args (cdr lst))
                       lst))))
  
  (define (rewrite-with-lemmas term lst)
    (cond ((null? lst)
           term)
          ((one-way-unify term (cadr (car lst)))
           (rewrite (apply-subst unify-subst (caddr (car lst)))))
          (else (rewrite-with-lemmas term (cdr lst)))))
  
  (define unify-subst '*)
  
  (define (one-way-unify term1 term2)
    (begin (set! unify-subst '())
           (one-way-unify1 term1 term2)))
  
  (define (one-way-unify1 term1 term2)
    (cond ((not (pair? term2))
           (let ((temp-temp (assq term2 unify-subst)))
             (cond (temp-temp
                    (term-equal? term1 (cdr temp-temp)))
                   ((number? term2)          ; This bug fix makes
                    (equal? term1 term2))    ; nboyer 10-25% slower!
                   (else
                    (set! unify-subst (cons (cons term2 term1)
                                            unify-subst))
                    #t))))
          ((not (pair? term1))
           #f)
          ((eq? (car term1)
                (car term2))
           (one-way-unify1-lst (cdr term1)
                               (cdr term2)))
          (else #f)))
  
  (define (one-way-unify1-lst lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((one-way-unify1 (car lst1)
                           (car lst2))
           (one-way-unify1-lst (cdr lst1)
                               (cdr lst2)))
          (else #f)))
  
  (define (falsep x lst)
    (or (term-equal? x false-term)
        (term-member? x lst)))
  
  (define (truep x lst)
    (or (term-equal? x true-term)
        (term-member? x lst)))
  
  (define false-term '*)  ; becomes (translate-term '(f))
  (define true-term '*)   ; becomes (translate-term '(t))
  
  ; The next two procedures were in the original benchmark
  ; but were never used.
  
  (define (trans-of-implies n)
    (translate-term
     (list (quote implies)
           (trans-of-implies1 n)
           (list (quote implies)
                 0 n))))
  
  (define (trans-of-implies1 n)
    (cond ((equal? n 1)
           (list (quote implies)
                 0 1))
          (else (list (quote and)
                      (list (quote implies)
                            (- n 1)
                            n)
                      (trans-of-implies1 (- n 1))))))
  
  ; Translated terms can be circular structures, which can't be
  ; compared using Scheme's equal? and member procedures, so we
  ; use these instead.
  
  (define (term-equal? x y)
    (cond ((pair? x)
           (and (pair? y)
                (symbol-record-equal? (car x) (car y))
                (term-args-equal? (cdr x) (cdr y))))
          (else (equal? x y))))
  
  (define (term-args-equal? lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((term-equal? (car lst1) (car lst2))
           (term-args-equal? (cdr lst1) (cdr lst2)))
          (else #f)))
  
  (define (term-member? x lst)
    (cond ((null? lst)
           #f)
          ((term-equal? x (car lst))
           #t)
          (else (term-member? x (cdr lst)))))
  
  (set! setup-boyer
        (lambda ()
          (set! *symbol-records-alist* '())
          (set! if-constructor (symbol->symbol-record 'if))
          (set! false-term (translate-term '(f)))
          (set! true-term  (translate-term '(t)))
          (setup)))
  
  (set! test-boyer
        (lambda (n)
          (set! rewrite-count 0)
          (let ((answer (test n)))
            (write rewrite-count)
            (display " rewrites")
            (newline)
            (if answer
                rewrite-count
                #f)))))
