(define (get-mad-libs file)
  (with-input-from-file file
    (lambda ()
      (for/fold ((text ""))
        ((line (in-lines)))
        (string-append text line "\n")))))


(define (replace-context mad-libs)
  (define matches
    (regexp-match* #rx"<[a-zA-Z0-9 ]*>" mad-libs))
  (map
   (lambda (context)
     (display (format "~a?" context))
     (cons context (read-line)))
   (remove-duplicates matches)))

(define (play-mad-libs)
  (display "Tell me a file to play Mad Libs: ")
  (define text (get-mad-libs (read-line)))
  (define matches (replace-context text))

  (display
   (for/fold ((mad-story text))
     ((change (in-list matches)))
     (string-replace mad-story (car change) (cdr change)))))

(play-mad-libs)
