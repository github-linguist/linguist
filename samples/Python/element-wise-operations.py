>>> import random
>>> from operator import add, sub, mul, floordiv
>>> from pprint import pprint as pp
>>>
>>> def ewise(matrix1, matrix2, op):
	return [[op(e1,e2) for e1,e2 in zip(row1, row2)] for row1,row2 in zip(matrix1, matrix2)]

>>> m,n = 3,4 	# array dimensions
>>> a0 = [[random.randint(1,9) for y in range(n)] for x in range(m)]
>>> a1 = [[random.randint(1,9) for y in range(n)] for x in range(m)]
>>> pp(a0); pp(a1)
[[7, 8, 7, 4], [4, 9, 4, 1], [2, 3, 6, 4]]
[[4, 5, 1, 6], [6, 8, 3, 4], [2, 2, 6, 3]]
>>> pp(ewise(a0, a1, add))
[[11, 13, 8, 10], [10, 17, 7, 5], [4, 5, 12, 7]]
>>> pp(ewise(a0, a1, sub))
[[3, 3, 6, -2], [-2, 1, 1, -3], [0, 1, 0, 1]]
>>> pp(ewise(a0, a1, mul))
[[28, 40, 7, 24], [24, 72, 12, 4], [4, 6, 36, 12]]
>>> pp(ewise(a0, a1, floordiv))
[[1, 1, 7, 0], [0, 1, 1, 0], [1, 1, 1, 1]]
>>> pp(ewise(a0, a1, pow))
[[2401, 32768, 7, 4096], [4096, 43046721, 64, 1], [4, 9, 46656, 64]]
>>> pp(ewise(a0, a1, lambda x, y:2*x - y))
[[10, 11, 13, 2], [2, 10, 5, -2], [2, 4, 6, 5]]
>>>
>>> def s_ewise(scalar1, matrix1, op):
	return [[op(scalar1, e1) for e1 in row1] for row1 in matrix1]

>>> scalar = 10
>>> a0
[[7, 8, 7, 4], [4, 9, 4, 1], [2, 3, 6, 4]]
>>> for op in ( add, sub, mul, floordiv, pow, lambda x, y:2*x - y ):
	print("%10s :" % op.__name__, s_ewise(scalar, a0, op))

	
       add : [[17, 18, 17, 14], [14, 19, 14, 11], [12, 13, 16, 14]]
       sub : [[3, 2, 3, 6], [6, 1, 6, 9], [8, 7, 4, 6]]
       mul : [[70, 80, 70, 40], [40, 90, 40, 10], [20, 30, 60, 40]]
  floordiv : [[1, 1, 1, 2], [2, 1, 2, 10], [5, 3, 1, 2]]
       pow : [[10000000, 100000000, 10000000, 10000], [10000, 1000000000, 10000, 10], [100, 1000, 1000000, 10000]]
  <lambda> : [[13, 12, 13, 16], [16, 11, 16, 19], [18, 17, 14, 16]]
>>>
