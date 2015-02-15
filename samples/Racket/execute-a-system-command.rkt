#lang racket

;; simple execution of a shell command
(system "ls")

;; capture output
(string-split (with-output-to-string (λ() (system "ls"))) "\n")

;; Warning: passing random string to be run in a shell is a bad idea!
;; much safer: avoids shell parsing, arguments passed separately
(system* "/bin/ls" "-l")

;; avoid specifying the executable path
(system* (find-executable-path "/bin/ls") "-l")
