#lang racket
(let loop ([nums (range 1 10)] [n 0])
  (cond [(apply < nums) (if (zero? n)
                            (loop (shuffle nums) 0)
                            (printf "Done in ~s steps.\n" n))]
        [else (printf "Step #~s: ~s\nFlip how many? " n nums)
              (define-values (l r) (split-at nums (read)))
              (loop (append (reverse l) r) (add1 n))]))
