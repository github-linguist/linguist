>>> def j(n, k):
	p, i, seq = list(range(n)), 0, []
	while p:
		i = (i+k-1) % len(p)
		seq.append(p.pop(i))
	return 'Prisoner killing order: %s.\nSurvivor: %i' % (', '.join(str(i) for i in seq[:-1]), seq[-1])

>>> print(j(5, 2))
Prisoner killing order: 1, 3, 0, 4.
Survivor: 2
>>> print(j(41, 3))
Prisoner killing order: 2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 0, 4, 9, 13, 18, 22, 27, 31, 36, 40, 6, 12, 19, 25, 33, 39, 7, 16, 28, 37, 10, 24, 1, 21, 3, 34, 15.
Survivor: 30
>>>
