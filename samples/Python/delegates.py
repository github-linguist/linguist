class Delegator:
   def __init__(self):
      self.delegate = None
   def operation(self):
       if hasattr(self.delegate, 'thing') and callable(self.delegate.thing):
          return self.delegate.thing()
       return 'default implementation'

class Delegate:
   def thing(self):
      return 'delegate implementation'

if __name__ == '__main__':

   # No delegate
   a = Delegator()
   assert a.operation() == 'default implementation'

   # With a delegate that does not implement "thing"
   a.delegate = 'A delegate may be any object'
   assert a.operation() == 'default implementation'

   # With delegate that implements "thing"
   a.delegate = Delegate()
   assert a.operation() == 'delegate implementation'
