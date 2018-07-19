#lang racket
(require srfi/13)
(string-prefix? "ab" "abcd")
(string-suffix? "cd" "abcd")
(string-contains "abab" "bb")
(string-contains "abab" "ba")
