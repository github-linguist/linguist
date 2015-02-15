#lang racket
(define input
#<<---
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
---
  )

(read (open-input-string input))
