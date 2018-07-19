#lang racket

(sort '(("UK" "London")
        ("US" "New York")
        ("US" "Birmingham")
        ("UK" "Birmingham"))
      string<? #:key first)
;; -> (("UK" "London") ("UK" "Birmingham")
;;     ("US" "New York") ("US" "Birmingham"))

(sort '(("UK" "London")
        ("US" "New York")
        ("US" "Birmingham")
        ("UK" "Birmingham"))
      string<? #:key second)
;; -> '(("US" "Birmingham") ("UK" "Birmingham")
;;      ("UK" "London") ("US" "New York"))
