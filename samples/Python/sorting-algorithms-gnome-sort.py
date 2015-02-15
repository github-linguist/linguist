>>> def gnomesort(a):
	i,j,size = 1,2,len(a)
	while i < size:
		if a[i-1] <= a[i]:
			i,j = j, j+1
		else:
			a[i-1],a[i] = a[i],a[i-1]
			i -= 1
			if i == 0:
				i,j = j, j+1
	return a

>>> gnomesort([3,4,2,5,1,6])
[1, 2, 3, 4, 5, 6]
>>>
