import sys, timeit
def usec(function, arguments):
    modname, funcname = __name__, function.__name__
    timer = timeit.Timer(stmt='%(funcname)s(*args)' % vars(),
                         setup='from %(modname)s import %(funcname)s; args=%(arguments)r' % vars())
    try:
        t, N = 0, 1
        while t < 0.2:
            t = min(timer.repeat(repeat=3, number=N))
            N *= 10
        microseconds = round(10000000 * t / N, 1) # per loop
        return microseconds
    except:
        timer.print_exc(file=sys.stderr)
        raise

 def nothing(): pass
 def identity(x): return x
