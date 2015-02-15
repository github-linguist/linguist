#lang racket
(require net/url)
(define *yaho-url* "http://search.yahoo.com/search?p=~a&b=~a")
(define *current-page* 0)
(define *current-query* "")
(define request (compose port->string get-pure-port string->url))

;;strip html tags
(define (remove-tags text)
  (regexp-replace* #px"<[^<]+?>" text ""))

;;search, parse and print
(define (search-yahoo query)
  (unless (string=? *current-query* query) ;different query, go back to page 1
    (set! *current-query* query)
    (set! *current-page* 0))
  (let* ([current-page (number->string (add1 (* 10 *current-page*)))]
         [html (request (format *yaho-url* query current-page))]
         [results (regexp-match* #px"lass=\"yschttl spt\" href=\".+?\">(.+?)<span class=url>(.+?)</span>.+?<div class=\"abstr\">(.+?)</div>" html #:match-select cdr)])
    (for ([result (in-list results)])
      (printf "Title: ~a \n Link: ~a \n Text: ~a \n\n"
              (remove-tags (first result))
              (remove-tags (second result) )
              (remove-tags (third result))))))

;;search nexxt page
(define (next-page)
  (set! *current-page* (add1 *current-page*))
  (search-yahoo *current-query*))
