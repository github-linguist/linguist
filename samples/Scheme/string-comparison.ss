;; Comparing two strings for exact equality
(string=? "hello" "hello")

;; Comparing two strings for inequality
(not (string=? "hello" "Hello"))

;; Checking if the first string is lexically ordered before the second
(string<? "bar" "foo")

;; Checking if the first string is lexically ordered after the second
(string>? "foo" "bar")

;; case insensitive comparison
(string-ci=? "hello" "Hello")
