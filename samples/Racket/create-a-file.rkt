#lang racket

(display-to-file "" "output.txt")
(make-directory "docs")
(display-to-file "" "/output.txt")
(make-directory "/docs")
