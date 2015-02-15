>>> class Borg(object):
	__state = {}
	def __init__(self):
		self.__dict__ = self.__state
	# Any other class names/methods

	
>>> b1 = Borg()
>>> b2 = Borg()
>>> b1 is b2
False
>>> b1.datum = range(5)
>>> b1.datum
[0, 1, 2, 3, 4]
>>> b2.datum
[0, 1, 2, 3, 4]
>>> b1.datum is b2.datum
True
>>> # For any datum!
