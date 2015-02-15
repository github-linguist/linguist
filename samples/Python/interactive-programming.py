python
Python 2.6.1 (r261:67517, Dec  4 2008, 16:51:00) [MSC v.1500 32 bit (Intel)] on
win32
Type "help", "copyright", "credits" or "license" for more information.
>>> def f(string1, string2, separator):
	return separator.join([string1, '', string2])

>>> f('Rosetta', 'Code', ':')
'Rosetta::Code'
>>>
