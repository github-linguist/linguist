(define haystack
  '("Zig" "Zag" "Wally" "Ronald" "Bush" "Krusty" "Charlie" "Bush" "Bozo"))

(define index-of
  (lambda (needle hackstack)
    (let ((tail (member needle haystack)))
      (if tail
          (- (length haystack) (length tail))
          (throw 'needle-missing)))))

(define last-index-of
  (lambda (needle hackstack)
    (let ((tail (member needle (reverse haystack))))
      (if tail
          (- (length tail) 1)
          (throw 'needle-missing)))))
