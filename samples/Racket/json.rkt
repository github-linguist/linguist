#lang racket

(require json)

(string->jsexpr
 "{\"foo\":[1,2,3],\"bar\":null,\"baz\":\"blah\"}")

(write-json '(1 2 "three" #hash((x . 1) (y . 2) (z . 3))))
