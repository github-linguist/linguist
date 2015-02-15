#lang racket

(module topic1 racket
  ;; define $ as a "parameter", but make it look like a plain identifier
  (provide $ (rename-out [$if if] [$#%app #%app]))
  (define current-value (make-parameter #f))
  (define-syntax $
    (syntax-id-rules (set!)
      [(_ x ...) ((current-value) x ...)]
      [(set! _ val) (current-value val)]
      [_ (current-value)]))
  ;; make an `if' form that binds it to the condition result
  (define-syntax-rule ($if C T E)
    (parameterize ([current-value C])
      (if $ T E)))
  ;; function application with []s uses the topic variable for the first arg
  (define-syntax ($#%app stx)
    (syntax-case stx ()
      [(_ f x y ...) (equal? #\[ (syntax-property stx 'paren-shape))
       #'(parameterize ([current-value x]) (f y ...))]
      [(_ f x ...) #'(f x ...)])))

(module topic2 racket
  ;; better variant: define `$' as a syntax parameter, which is adjusted to an
  ;; actual local binding; make it work in `if', and have a function definition
  ;; form that binds it to the actual arguments
  (provide $ (rename-out [$if if]) defun)
  (require racket/stxparam)
  (define-syntax-parameter $ (λ(stx) (raise-syntax-error '$ "not in scope")))
  (define-syntax-rule ($if C T E)
    (let ([c C]) (syntax-parameterize ([$ (make-rename-transformer #'c)])
                   (if c T E))))
  (define-syntax-rule (defun name body ...)
    (define (name arg)
      (syntax-parameterize ([$ (make-rename-transformer #'arg)])
        body ...)))
  )

(module sample1 racket
  (require (submod ".." topic1))
  (if (memq 2 '(1 2 3)) (cadr $) 'missing)
  ;; => 3
  (define (foo) (list (sqrt $) (* $ $)))
  [foo 9]
  ;; => '(3 81)
  )
(require 'sample1)

(module sample2 racket
  (require (submod ".." topic2))
  (if (memq 2 '(1 2 3)) (cadr $) 'missing)
  ;; => 3
  (defun foo (list (sqrt $) (* $ $)))
  (foo 9)
  ;; => '(3 81)
  )
(require 'sample2)
