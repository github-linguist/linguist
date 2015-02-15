#lang racket
(define-struct dlist (head tail) #:mutable #:transparent)
(define-struct dlink (content prev next) #:mutable #:transparent)

(define (insert-between dlist before after data)
  ; Insert a fresh link containing DATA after existing link
  ; BEFORE if not nil and before existing link AFTER if not nil
  (define new-link (make-dlink data before after))
  (if before
      (set-dlink-next! before new-link)
      (set-dlist-head! dlist new-link))
  (if after
      (set-dlink-prev! after new-link)
      (set-dlist-tail! dlist new-link))
    new-link)

(define (insert-before dlist dlink data)
  ; Insert a fresh link containing DATA before existing link DLINK
  (insert-between dlist (dlink-prev dlink) dlink data))

(define (insert-after dlist dlink data)
  ; Insert a fresh link containing DATA after existing link DLINK
  (insert-between dlist dlink (dlink-next dlink) data))

(define (insert-head dlist data)
  ; Insert a fresh link containing DATA at the head of DLIST
  (insert-between dlist #f (dlist-head dlist) data))

(define (insert-tail dlist data)
  ; Insert a fresh link containing DATA at the tail of DLIST
  (insert-between dlist (dlist-tail dlist) #f data))

(define (remove-link dlist dlink)
  ; Remove link DLINK from DLIST and return its content
  (let ((before (dlink-prev dlink))
        (after (dlink-next dlink)))
    (if before
        (set-dlink-next! before after)
        (set-dlist-head! dlist after))
    (if after
        (set-dlink-prev! after before)
        (set-dlist-tail! dlist before))))

(define (dlist-elements dlist)
  ; Returns the elements of DLIST as a list
  (define (extract-values dlink acc)
    (if dlink
        (extract-values (dlink-next dlink) (cons (dlink-content dlink) acc))
        acc))
  (reverse (extract-values (dlist-head dlist) '())))

(let ((dlist (make-dlist #f #f)))
  (insert-head dlist 1)
  (insert-tail dlist 4)
  (insert-after dlist (dlist-head dlist) 2)
  (let* ((next-to-last (insert-before dlist (dlist-tail dlist) 3))
         (bad-link (insert-before dlist next-to-last 42)))
    (remove-link dlist bad-link))
  (dlist-elements dlist))
