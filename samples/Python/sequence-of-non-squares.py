>>> from math import sqrt
>>> # (Using the fact that round(X) is equivalent to floor(0.5+X) for our range of X)
>>> def nonsqr(n): return n + int(round(sqrt(n)))

>>> # first 22 values (as a list) has no squares:
>>> [nonsqr(i) for i in xrange(1,23)]
[2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27]
>>> # The following check shows no squares up to one million:
>>> for i in xrange(1,1000000):
	j = sqrt(nonsqr(i))
	assert j != int(j), "Found a square in the sequence: %i" % i

	
>>>
