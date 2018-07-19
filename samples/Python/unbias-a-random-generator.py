from __future__ import print_function
import random

def randN(N):
    " 1,0 random generator factory with 1 appearing 1/N'th of the time"
    return lambda: random.randrange(N) == 0

def unbiased(biased):
    'uses a biased() generator of 1 or 0, to create an unbiased one'
    this, that = biased(), biased()
    while this == that: # Loop until 10 or 01
        this, that = biased(), biased()
    return this         # return the first

if __name__ == '__main__':
    from collections import namedtuple

    Stats = namedtuple('Stats', 'count1 count0 percent')

    for N in range(3, 7):
        biased = randN(N)
        v = [biased() for x in range(1000000)]
        v1, v0 = v.count(1), v.count(0)
        print ( "Biased(%i)  = %r" % (N, Stats(v1, v0, 100. * v1/(v1 + v0))) )

        v = [unbiased(biased) for x in range(1000000)]
        v1, v0 = v.count(1), v.count(0)
        print ( "  Unbiased = %r" % (Stats(v1, v0, 100. * v1/(v1 + v0)), ) )
