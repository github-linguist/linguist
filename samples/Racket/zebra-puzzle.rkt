#lang racket

(require racklog)

(define %select
  (%rel (x xs S S1)
    [(x (cons x xs) xs)]
    [(x (cons S xs) (cons S S1)) (%select x xs S1)]
    [((cons x xs) S)
     (%select x S S1)
     (%select xs S1)]
    [('() (_))]))

(define %next-to
  (%rel (A B C)
    [(A B C)
     (%or (%left-of A B C)
          (%left-of B A C))]))

(define %left-of
  (%rel (A B C)
    [(A B C) (%append (_) (cons A (cons B (_))) C)]))

(define %zebra
  (%rel (Owns HS)
    [(Owns HS)
     (%is HS (list (list (_) 'norwegian (_) (_) (_))
                   (_)
                   (list (_) (_) (_) 'milk (_))
                   (_) (_)))
     (%select (list (list 'red 'englishman (_) (_) (_))
                    (list (_) 'swede 'dog (_) (_))
                    (list (_) 'dane (_) 'tea (_))
                    (list (_) 'german (_) (_) 'prince))
              HS)
     (%select (list (list (_) (_) 'birds (_) 'pallmall)
                    (list 'yellow (_) (_) (_) 'dunhill)
                    (list (_) (_) (_) 'beer 'bluemaster))
              HS)
     (%left-of (list 'green (_) (_) 'coffee (_))
               (list 'white (_) (_) (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'dunhill)
               (list (_) (_) 'horse (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'blend)
               (list (_) (_) 'cats (_) (_))
               HS)
     (%next-to (list (_) (_) (_) (_) 'blend)
               (list (_) (_) (_) 'water (_))
               HS)
     (%next-to (list (_) 'norwegian (_) (_) (_))
               (list 'blue (_) (_) (_) (_))
               HS)
     (%member (list (_) Owns 'zebra (_) (_)) HS)]))

(%which (Who HS) (%zebra Who HS))
