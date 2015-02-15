#lang racket

(require net/url net/url-connect openssl)

(module+ main
  (parameterize ([current-https-protocol (ssl-make-client-context)])
    (ssl-set-verify! (current-https-protocol) #t)

    ;; When this is #f, we correctly get an exception:
    ;; error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    (when #t
      (ssl-load-verify-source! (current-https-protocol)
                               '(directory
                                 ;; This location works on Debian 6;
                                 ;; adjust as needed for your platform.
                                 "/etc/ssl/certs"
                                 )))

    (for ([l (in-port read-line (get-pure-port (string->url "https://www.google.com/")))])
      (displayln  l))))
