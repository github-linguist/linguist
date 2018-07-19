; supply comparison function, which returns true if first and second
; arguments are in order or equal.
(define (gnome-sort-compar in-order input-list)
  (let gnome ((p (list (car input-list)))
              (n (cdr input-list)))
    (if (null? n) ; no more flowerpots?
        p ; we're done
        (let ((prev-pot (car p))
              (next-pot (car n)))
          (if (in-order next-pot prev-pot)
              ; if the pots are in order, step forwards.
              ; otherwise, exchange the two pots, and step backwards.
              (gnome (cons next-pot p) ; Prev list grows
                     (cdr n)) ; Next list shorter by one
              (if (null? (cdr p)) ; are we at the beginning?
                  (gnome                    ; if so, we can't step back
                   (list next-pot)          ; simply exchange the pots without
                   (cons prev-pot (cdr n))) ; changing lengths of lists
                  (gnome
                   (cdr p) ; Prev list shorter by one
                   (cons next-pot (cons prev-pot (cdr n))))))))))
