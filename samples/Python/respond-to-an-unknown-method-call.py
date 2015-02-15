class Example(object):
    def foo(self):
        print("this is foo")
    def bar(self):
        print("this is bar")
    def __getattr__(self, name):
        def method(*args):
            print("tried to handle unknown method " + name)
            if args:
                print("it had arguments: " + str(args))
        return method

example = Example()

example.foo()        # prints “this is foo”
example.bar()        # prints “this is bar”
example.grill()      # prints “tried to handle unknown method grill”
example.ding("dong") # prints “tried to handle unknown method ding”
                     # prints “it had arguments: ('dong',)”
