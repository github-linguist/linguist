#lang racket
(require file/md5)

(md5 "")
(md5 "a")
(md5 "abc")
(md5 "message digest")
(md5 "abcdefghijklmnopqrstuvwxyz")
(md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
(md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
