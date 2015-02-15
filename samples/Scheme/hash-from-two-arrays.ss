(define (lists->hash-table keys values . rest)
  (apply alist->hash-table (map cons keys values) rest))
