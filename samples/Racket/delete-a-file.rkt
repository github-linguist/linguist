#lang racket

;; here
(delete-file "input.txt")
(delete-directory "docs")
(delete-directory/files "docs") ; recursive deletion

;; in the root
(delete-file "/input.txt")
(delete-directory "/docs")
(delete-directory/files "/docs")

;; or in the root with relative paths
(parameterize ([current-directory "/"])
  (delete-file "input.txt")
  (delete-directory "docs")
  (delete-directory/files "docs"))
