#lang racket
(require db)

(define con (postgresql-connect #:user user #:database db #:password password))
(define pst (prepare pgc "UPDATE players
                            SET name = ?, score = ?, active = ?
                            WHERE jerseyNum = ?"))
(define bst (bind-prepared-statement pst '("Smith, Steve" 42 #t 99)))
(query-value con bst)
