(define-library (libs basic)
    (export list2 x)
    (begin
        (define (list2 . objs)  objs)
        (define x 'libs-basic)
        (define not-exported 'should-not-be-exported)
        ))
