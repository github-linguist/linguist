#lang racket
(require racket/lazy-require)
(lazy-require [ffi/com (com-create-instance com-release com-invoke)])
(define (speak text)
  (cond [(eq? 'windows (system-type))
         (define c (com-create-instance "SAPI.SpVoice"))
         (com-invoke c "Speak" text)
         (com-release c)]
        [(ormap find-executable-path '("say" "espeak"))
         => (λ(exe) (void (system* exe text)))]
        [else (error 'speak "I'm speechless!")]))
(speak "This is an example of speech synthesis.")
