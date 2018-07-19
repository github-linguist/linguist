>>> from operator import mul
>>> def matrixMul(m1, m2):
  return map(
    lambda row:
      map(
        lambda *column:
          sum(map(mul, row, column)),
        *m2),
    m1)

>>> def identity(size):
	size = range(size)
	return [[(i==j)*1 for i in size] for j in size]

>>> def matrixExp(m, pow):
	assert pow>=0 and int(pow)==pow, "Only non-negative, integer powers allowed"
	accumulator = identity(len(m))
	for i in range(pow):
		accumulator = matrixMul(accumulator, m)
	return accumulator

>>> def printtable(data):
	for row in data:
		print ' '.join('%-5s' % ('%s' % cell) for cell in row)

		
>>> m = [[3,2], [2,1]]
>>> for i in range(5):
	print '\n%i:' % i
	printtable( matrixExp(m, i) )

	

0:
1     0
0     1

1:
3     2
2     1

2:
13    8
8     5

3:
55    34
34    21

4:
233   144
144   89
>>> printtable( matrixExp(m, 10) )
1346269 832040
832040 514229
>>>
