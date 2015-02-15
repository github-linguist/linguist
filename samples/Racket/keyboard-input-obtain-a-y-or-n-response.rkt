#lang racket

;; GUI version
(require racket/gui)
(message-box "Yes/No example" "Yes or no?" #f '(yes-no))

;; Text version, via stty
(define stty
  (let ([exe (find-executable-path "stty")])
    (λ args (void (apply system* exe args)))))
(define tty-settings (string-trim (with-output-to-string (λ() (stty "-g")))))
(printf "Yes or no? ") (flush-output)
(stty "-icanon" "-echo" "min" "1")
(let loop () (when (char-ready?) (loop)))
(let loop ()
  (define ch (read-char))
  (case (char-downcase ch)
    [(#\y #\Y #\n #\N) (displayln ch) (if (memq ch '(#\y #\Y)) 'yes 'no)]
    [else (loop)]))
(stty tty-settings)
