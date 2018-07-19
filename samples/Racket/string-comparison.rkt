#lang racket

;; Comparing two strings for exact equality
(string=? "foo" "foo")

;; Comparing two strings for inequality
(not (string=? "foo" "bar"))

;; Comparing two strings to see if one is lexically ordered before than the other
(string<? "abc" "def")

;; Comparing two strings to see if one is lexically ordered after than the other
(string>? "def" "abc")

;; How to achieve both case sensitive comparisons and case insensitive comparisons within the language
(string-ci=? "foo" "FOO")
