import traceback

def f(): return g()
def g(): traceback.print_stack()

f()
