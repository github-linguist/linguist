class U0(Exception): pass
class U1(Exception): pass

def foo():
    for i in range(2):
        try:
            bar(i)
        except U0:
            print("Function foo caught exception U0")

def bar(i):
    baz(i) # Nest those calls

def baz(i):
    raise U1 if i else U0

foo()
