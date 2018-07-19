(define (square n)
  (* n n))

(define (mod-exp a n mod)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (mod-exp a (/ n 2) mod))
                    mod))
        (else (remainder (* a (mod-exp a (- n 1) mod))
                         mod))))

(define result
  (mod-exp 2988348162058574136915891421498819466320163312926952423791023078876139
           2351399303373464486466122544523690094744975233415544072992656881240319
           (expt 10 40)))
