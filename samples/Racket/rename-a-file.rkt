#lang racket

(rename-file-or-directory "input.txt" "output.txt")
(rename-file-or-directory "docs" "mydocs")

;; find the filesystem roots, and pick the first one
(define root (first (filesystem-root-list)))

(rename-file-or-directory (build-path root "input.txt")
                          (build-path root "output.txt"))
(rename-file-or-directory (build-path root "docs")
                          (build-path root "mydocs"))
