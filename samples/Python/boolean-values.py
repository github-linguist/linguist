>>> True
True
>>> not True
False
>>> # As numbers
>>> False + 0
0
>>> True + 0
1
>>> False + 0j
0j
>>> True * 3.141
3.141
>>> # Numbers as booleans
>>> not 0
True
>>> not not 0
False
>>> not 1234
False
>>> bool(0.0)
False
>>> bool(0j)
False
>>> bool(1+2j)
True
>>> # Collections as booleans
>>> bool([])
False
>>> bool([None])
True
>>> 'I contain something' if (None,) else 'I am empty'
'I contain something'
>>> bool({})
False
>>> bool("")
False
>>> bool("False")
True
