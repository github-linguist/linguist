from __future__ import division # Only necessary for Python 2.X
from math import factorial
from random import randrange

MAX_N = 20
TIMES = 1000000

def analytical(n):
	return sum(factorial(n) / pow(n, i) / factorial(n -i) for i in range(1, n+1))

def test(n, times):
    count = 0
    for i in range(times):
        x, bits = 1, 0
        while not (bits & x):
            count += 1
            bits |= x
            x = 1 << randrange(n)
    return count / times

if __name__ == '__main__':
    print(" n\tavg\texp.\tdiff\n-------------------------------")
    for n in range(1, MAX_N+1):
        avg = test(n, TIMES)
        theory = analytical(n)
        diff = (avg / theory - 1) * 100
        print("%2d %8.4f %8.4f %6.3f%%" % (n, avg, theory, diff))
