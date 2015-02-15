(let ((array #(1 2 3 4 5))     ; vector literal
      (array2 (make-vector 5))  ; default is unspecified
      (array3 (make-vector 5 0))) ; default 0
 (vector-set! array 0 3)
 (vector-ref array 0))    ; 3
