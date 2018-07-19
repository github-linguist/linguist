>>> from math import sqrt
>>> def qmean(num):
	return sqrt(sum(n*n for n in num)/len(num))

>>> qmean(range(1,11))
6.2048368229954285
