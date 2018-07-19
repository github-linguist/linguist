(define split-into-lines
  (lambda (str)
    (let loop ((index 0)
               (result '()))
      (let ((next-index (string-index str #\newline index)))
        (if next-index
            (loop (+ next-index 1)
                  (cons (substring str index next-index) result))
            (reverse (cons (substring str index) result)))))))

(define parse-rules
  (lambda (str)
    (let loop ((rules (split-into-lines str))
               (result '()))
      (if (null? rules)
          (reverse result)
          (let ((rule (car rules)))
            (loop (cdr rules)
                  (if (or (string=? rule "")
                          (eq? (string-ref rule 0) #\#))
                      result
                      (cons
                       (let ((index (string-contains rule "->" 1)))
                         (list (string-trim-right (substring rule 0 index))
                               (string-trim (substring rule (+ index 2)))))
                       result))))))))


(define apply-rules
  (lambda (str rules)
    (let loop ((remaining rules)
               (result str))
      (if (null? remaining)
          result
          (let* ((rule (car remaining))
                 (pattern (car rule))
                 (replacement (cadr rule))
                 (start (string-contains result pattern)))
            (if start
                (if (eq? #\. (string-ref replacement 0))
                    (string-replace result replacement start
                                    (+ start (string-length pattern)) 1)
                    (apply-rules
                     (string-replace result replacement start
                                     (+ start (string-length pattern)))
                     rules))
                (loop (cdr remaining) result)))))))
