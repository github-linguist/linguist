>>> class MyClassName:
	__private = 123
	non_private = __private * 2

	
>>> mine = MyClassName()
>>> mine.non_private
246
>>> mine.__private
Traceback (most recent call last):
  File "<pyshell#23>", line 1, in <module>
    mine.__private
AttributeError: 'MyClassName' object has no attribute '__private'
>>> mine._MyClassName__private
123
>>>
