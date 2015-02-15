>>> def sort_disjoint_sublist(data, indices):
	indices = sorted(indices)
	values  = sorted(data[i] for i in indices)
	for index, value in zip(indices, values):
		data[index] = value

		
>>> d = [7, 6, 5, 4, 3, 2, 1, 0]
>>> i = set([6, 1, 7])
>>> sort_disjoint_sublist(d, i)
>>> d
[7, 0, 5, 4, 3, 2, 1, 6]
>>> # Which could be more cryptically written as:
>>> def sort_disjoint_sublist(data, indices):
	for index, value in zip(sorted(indices), sorted(data[i] for i in indices)): data[index] = value

	
>>>
