class Node(object):
     def __init__(self, data = None, prev = None, next = None):
         self.prev = prev
         self.next = next
         self.data = data
     def __str__(self):
         return str(self.data)
     def __repr__(self):
         return repr(self.data)
     def iter_forward(self):
         c = self
         while c != None:
             yield c
             c = c.next
     def iter_backward(self):
         c = self
         while c != None:
             yield c
             c = c.prev
