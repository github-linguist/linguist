(define (palindromb str)
  (let* ([lst (string->list (string-downcase str))]
         [slst (remove* '(#\space) lst)])
    (string=? (list->string (reverse slst)) (list->string slst))))

;;example output

> (palindromb "able was i ere i saw elba")
#t
> (palindromb "waht the hey")
#f
> (palindromb "In girum imus nocte et consumimur igni")
#t
>
