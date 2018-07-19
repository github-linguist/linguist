#lang racket

(require net/dns)
(dns-get-address "8.8.8.8" "www.kame.net")
