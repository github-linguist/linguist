#lang racket

;; here
(file-exists? "input.txt")
(file-exists? "docs")

;; in the root
(file-exists? "/input.txt")
(file-exists? "/docs")

;; or in the root with relative paths
(parameterize ([current-directory "/"])
  (and (file-exists? "input.txt")
       (file-exists? "docs")))
