class LinkedList(object):
     """USELESS academic/classroom example of a linked list implemented in Python.
        Don't ever consider using something this crude!  Use the built-in list() type!
     """
	class Node(object):
		def __init__(self, item):
			self.value  = item
			self.next = None
	def __init__(self, item=None):
		if item is not None:
			self.head = Node(item); self.tail = self.head
		else:
			self.head = None; self.tail = None
	def append(self, item):
		if not self.head:
			self.head = Node(item)
			self.tail = self.head
		elif self.tail:
			self.tail.next = Node(item)
			self.tail = self.tail.next
		else:
			self.tail = Node(item)
	def __iter__(self):
		cursor = self.head
		while cursor:
			yield cursor.value
			cursor = cursor.next
