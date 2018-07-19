;; range and filter create lazy seq's
(filter even? (range 0 100))
;; vec will convert any type of seq to an array
(vec (filter even? (vec (range 0 100))))
