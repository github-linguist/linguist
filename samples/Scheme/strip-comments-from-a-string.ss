(use-modules (ice-9 regex))

(define (strip-comments s)
    (regexp-substitute #f
        (string-match "[ \t\r\n\v\f]*[#;].*" s) 'pre "" 'post))

(display (strip-comments "apples, pears # and bananas"))(newline)
(display (strip-comments "apples, pears ; and bananas"))(newline)
