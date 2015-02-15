#lang racket

(require net/url net/uri-codec json (only-in racket/dict [dict-ref ref]))

(define (RC-get verb params)
  ((compose1 get-pure-port string->url format)
   "http://rosettacode.org/mw/~a.php?~a" verb (alist->form-urlencoded params)))

(define (get-category catname)
  (let loop ([c #f])
    (define t
      ((compose1 read-json RC-get) 'api
       `([action . "query"] [format . "json"]
         [list . "categorymembers"] [cmtitle . ,(format "Category:~a" catname)]
         [cmcontinue . ,(and c (ref c 'cmcontinue))] [cmlimit . "500"])))
    (define (c-m key) (ref (ref t key '()) 'categorymembers #f))
    (append (for/list ([page (c-m 'query)]) (ref page 'title))
            (cond [(c-m 'query-continue) => loop] [else '()]))))

;; The above is the required "library" code, same as the "Rosetta
;; Code/Count" entry

(define (show-unimplemented lang)
  (for-each displayln (remove* (get-category lang)
                               (get-category 'Programming_Tasks))))

(show-unimplemented 'Racket) ; see all of the Racket entries
