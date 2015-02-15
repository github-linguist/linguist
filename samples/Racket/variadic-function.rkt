-> (define (vfun . xs) (for-each displayln xs))
-> (vfun)
-> (vfun 1)
1
-> (vfun 1 2 3 4)
1
2
3
4
-> (apply vfun (range 10 15))
10
11
12
13
14
