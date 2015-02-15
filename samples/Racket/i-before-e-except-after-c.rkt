#lang racket

(define (get-tallies filename line-parser . patterns)
  (for/fold ([totals (make-list (length patterns) 0)])
    ([line (file->lines filename)])
    (match-let ([(list word n) (line-parser line)])
      (for/list ([p patterns] [t totals])
        (if (regexp-match? p word)
            (+ n t) t)))))

(define (plausible test) (string-append (if test "" "IM") "PLAUSIBLE"))

(define (subrule description examples counters)
  (let ([result (> examples (* 2 counters))])
    (printf "  The sub-rule \"~a\" is ~a.  There were ~a examples and ~a counter-examples.\n"
            description (plausible result) examples counters)
    result))

(define (plausibility description filename parser)
  (printf "~a:\n" description)
  (match-let ([(list cei cie ie ei) (get-tallies filename parser "cei" "cie" "ie" "ei")])
    (let ([rule1 (subrule "I before E when not preceded by C" (- ie cie) (- ei cei))]
          [rule2 (subrule "E before I when preceded by C" cei cie)])
      (printf "\n  Overall, the rule \"I before E, except after C\" is ~a.\n"
              (plausible (and rule1 rule2))))))

(define (parse-frequency-data line)
  (let ([words (string-split line)])
    (list (string-join (drop-right words 2)) (string->number (last words)))))

(plausibility "Dictionary" "unixdict.txt" (λ (line) (list line 1))) (newline)
(plausibility "Word frequencies (stretch goal)" "1_2_all_freq.txt" parse-frequency-data)
