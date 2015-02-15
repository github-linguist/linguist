#lang racket

(let* ([s1 "Hey"]
       [s2 s1]
       [s3 (string-copy s1)]
       [s4 s3])
  (printf "s1 and s2 refer to ~a strings\n"
          (if (eq? s1 s2) "the same" "different")) ; same
  (printf "s1 and s3 refer to ~a strings\n"
          (if (eq? s1 s3) "the same" "different")) ; different
  (printf "s3 and s4 refer to ~a strings\n"
          (if (eq? s3 s4) "the same" "different")) ; same
  (string-fill! s3 #\!)
  (printf "~a~a~a~a\n" s1 s2 s3 s4)) ; outputs "HeyHey!!!!!!"
