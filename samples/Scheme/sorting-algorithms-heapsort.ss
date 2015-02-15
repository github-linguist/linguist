; swap two elements of a vector
(define (swap! v i j)
  (define temp (vector-ref v i))
  (vector-set! v i (vector-ref v j))
  (vector-set! v j temp))

; sift element at node start into place
(define (sift-down! v start end)
  (let ((child (+ (* start 2) 1)))
    (cond
      ((> child end) 'done) ; start has no children
      (else
       (begin
         ; if child has a sibling node whose value is greater ...
         (and (and (<= (+ child 1) end)
                   (< (vector-ref v child) (vector-ref v (+ child 1))))
              ; ... then we'll look at the sibling instead
              (set! child (+ child 1)))
         (if (< (vector-ref v start) (vector-ref v child))
             (begin
               (swap! v start child)
               (sift-down! v child end))
             'done))))))

; transform v into a binary max-heap
(define (heapify v)
  (define (iter v start)
    (if (>= start 0)
        (begin (sift-down! v start (- (vector-length v) 1))
               (iter v (- start 1)))
        'done))
  ; start sifting with final parent node of v
  (iter v (quotient (- (vector-length v) 2) 2)))

(define (heapsort v)
  ; swap root and end node values,
  ; sift the first element into place
  ; and recurse with new root and next-to-end node
  (define (iter v end)
    (if (zero? end)
        'done
        (begin
          (swap! v 0 end)
          (sift-down! v 0 (- end 1))
          (iter v (- end 1)))))
  (begin
    (heapify v)
    ; start swapping with root and final node
    (iter v (- (vector-length v) 1))))

; testing
(define uriah (list->vector '(3 5 7 9 0 8 1 4 2 6)))
(heapsort uriah)
uriah
