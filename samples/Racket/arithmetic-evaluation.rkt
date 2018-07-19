#lang racket

(require parser-tools/yacc parser-tools/lex
         (prefix-in ~ parser-tools/lex-sre))

(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (OPEN CLOSE + - * / EOF NEG))

(define lex
  (lexer [(eof) 'EOF]
         [whitespace (lex input-port)]
         [(~or "+" "-" "*" "/") (string->symbol lexeme)]
         ["(" 'OPEN]
         [")" 'CLOSE]
         [(~: (~+ numeric) (~? #\. (~* numeric)))
          (token-NUM (string->number lexeme))]))

(define parse
  (parser [start E] [end EOF]
          [tokens value-tokens op-tokens]
          [error void]
          [precs (left - +) (left * /) (left NEG)]
          [grammar (E [(NUM) $1]
                      [(E + E) (+ $1 $3)]
                      [(E - E) (- $1 $3)]
                      [(E * E) (* $1 $3)]
                      [(E / E) (/ $1 $3)]
                      [(- E) (prec NEG) (- $2)]
                      [(OPEN E CLOSE) $2])]))

(define (calc str)
  (define i (open-input-string str))
  (displayln (parse (λ() (lex i)))))

(calc "(1 + 2 * 3) - (1+2)*-3")
