class Example(object):
     def foo(self, x):
             return 42 + x

name = "foo"
getattr(Example(), name)(5)      # => 47
