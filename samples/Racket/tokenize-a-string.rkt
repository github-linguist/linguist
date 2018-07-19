#lang racket
(string-join (string-split "Hello,How,Are,You,Today" ",") ".")
;; -> "Hello.How.Are.You.Today"
