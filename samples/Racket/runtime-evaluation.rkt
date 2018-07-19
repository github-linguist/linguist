#lang racket
(require racket/sandbox)
(define e (make-evaluator 'racket))
(e '(define + *))
(e '(+ 10 20))
(+ 10 20)
;; (e '(delete-file "/etc/passwd"))
;; --> delete-file: `delete' access denied for /etc/passwd
