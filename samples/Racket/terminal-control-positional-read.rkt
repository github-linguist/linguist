#lang racket
(require ffi/unsafe ffi/unsafe/define)
(define-ffi-definer defwin #f)
(defwin GetStdHandle (_fun _int -> _pointer))
(defwin ReadConsoleOutputCharacterA
  (_fun _pointer _pointer _uint _uint [len : (_ptr o _uint)] -> _bool))

(define b (make-bytes 1 32))
(and (ReadConsoleOutputCharacterA (GetStdHandle -11) b 1 #x50002)
     (printf "The character at 3x6 is <~a>\n" b))
