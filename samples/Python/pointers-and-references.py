 # Bind a literal string object to a name:
 a = "foo"
 # Bind an empty list to another name:
 b = []
 # Classes are "factories" for creating new objects: invoke class name as a function:
 class Foo(object):
     pass
 c = Foo()
 # Again, but with optional initialization:
 class Bar(object):
     def __init__(self, initializer = None)
         # "initializer is an arbitrary identifier, and "None" is an arbitrary default value
         if initializer is not None:
            self.value = initializer
 d = Bar(10)
 print d.value
 # Test if two names are references to the same object:
 if a is b: pass
 # Alternatively:
 if id(a) == id(b): pass
 # Re-bind a previous used name to a function:
 def a(fmt, *args):
     if fmt is None:
         fmt = "%s"
      print fmt % (args)
 # Append reference to a list:
 b.append(a)
 # Unbind a reference:
 del(a)
 # Call (anymous function object) from inside a list
 b[0]("foo")  # Note that the function object we original bound to the name "a" continues to exist
              # even if its name is unbound or rebound to some other object.
