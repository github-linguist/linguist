#lang racket

(require net/url)
(require json)

(define proglangs_url "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmtitle=Category:Programming_Languages&cmlimit=500&format=json")
(define categories_url "http://rosettacode.org/mw/index.php?title=Special:Categories&limit=5000")

(define (fetch-json urlstring)
  (read-json (get-pure-port (string->url urlstring))))

(define programming-languages
  (for/set ([h (in-list
                (hash-ref (hash-ref (fetch-json proglangs_url) 'query)
                          'categorymembers))])
           (substring (hash-ref h 'title) 9)))

(define result '())
(for ([l (in-port read-line (get-pure-port (string->url categories_url)))])
  (let ([res (regexp-match #px"title=\"Category:(.+?)\".+\\((\\d+) member" l)])
    (when (and res (set-member? programming-languages (cadr res)))
      (set! result (cons (cons (cadr res)
                               (string->number (caddr res)))
                         result)))))

(printf "Place\tCount\tName~n")
(for ([lang (in-list (sort result > #:key cdr))]
      [place (in-naturals 1)])
  (printf "~a\t~a\t~a~n" place (cdr lang) (car lang)))
