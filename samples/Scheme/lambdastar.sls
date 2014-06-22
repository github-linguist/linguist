(library (lambdastar)
  (export (rename (lambda* lambda)))
  (import (rnrs))

(define-syntax lambda*
  (syntax-rules ()
    ((_ a* e* ...)
     ( lambda*-h a* (let () e* ...)))))
  
(define-syntax lambda*-h
  (syntax-rules ()
    ((_ () e)
     (lambda a* (if (null? a*) e (apply (e) a*))))
    ((_ (a a* ...) e) (posary-h (a a* ...) e))
    ((_ (a a* ... . rest) e)
     (polyvariadic-h (a a* ... . rest) e))
    ((_ a* e) (lambda a* e))))

(define-syntax posary-h
  (syntax-rules ()
    ((_ (a a* ...) e)
     (letrec
         ((rec
           (case-lambda
               (() rec)
             ((a a* ...) e)
             ((a a* ... . rest)
              (apply (rec a a* ...) rest))
             (some (get-more rec some)))))
       rec))))

(define-syntax polyvariadic-h
  (syntax-rules ()
    ((_ (a a* ... . rest) e)
     (letrec
       ((rec
         (case-lambda
           (() rec)
           ((a a* ... . rest) e)
           (some (get-more rec some)))))
       rec))))

(define get-more
  (lambda (rec some)
     (lambda more 
       (apply rec (append some more))))))