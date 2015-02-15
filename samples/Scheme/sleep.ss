(use format)
(use srfi-18)

(format #t "Enter a time (in seconds): ")
(let ((time (read))) ; converts input to a number, if possible
  (if (number? time)
    (begin
      (format #t "Sleeping...~&")
      (thread-sleep! time)
      (format #t "Awake!~&"))
    (format #t "You must enter a number~&")))
