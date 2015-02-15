#lang racket

;; using sendmail:
(require net/sendmail)
(send-mail-message
 "sender@somewhere.com" "Some Subject"
 '("recipient@elsewhere.com" "recipient2@elsewhere.com")
 '("cc@elsewhere.com")
 '("bcc@elsewhere.com")
 (list "Some lines of text" "go here."))

;; and using smtp (and adding more headers here):
(require net/head net/smtp)
(smtp-send-message
 "192.168.0.1"
 "Sender <sender@somewhere.com>"
 '("Recipient <recipient@elsewhere.com>")
 (standard-message-header
  "Sender <sender@somewhere.com>"
  '("Recipient <recipient@elsewhere.com>")
  '() ; CC
  '() ; BCC
  "Subject")
 '("Hello World!"))
