>>> from array import array
>>> argslist = [('l', []), ('c', 'hello world'), ('u', u'hello \u2641'),
	('l', [1, 2, 3, 4, 5]), ('d', [1.0, 2.0, 3.14])]
>>> for typecode, initializer in argslist:
	a = array(typecode, initializer)
	print a
	del a

	
array('l')
array('c', 'hello world')
array('u', u'hello \u2641')
array('l', [1, 2, 3, 4, 5])
array('d', [1.0, 2.0, 3.1400000000000001])
>>>
