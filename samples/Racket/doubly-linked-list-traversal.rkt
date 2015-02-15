(define (dlist-elements dlist)
  (let loop ([elements '()] [dlink (dlist-tail dlist)])
    (if dlink
        (loop (cons (dlink-content dlink) elements) (dlink-prev dlink))
        elements)))

(define (dlist-elements/reverse dlist)
  (let loop ([elements '()] [dlink (dlist-head dlist)])
    (if dlink
        (loop (cons (dlink-content dlink) elements) (dlink-next dlink))
        elements)))
