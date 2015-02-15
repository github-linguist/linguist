#lang racket/base

(printf "Word size: ~a\n" (system-type 'word))
(printf "Endianness: ~a\n" (if (system-big-endian?) 'big 'little))
