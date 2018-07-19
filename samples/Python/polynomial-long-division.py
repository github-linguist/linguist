# -*- coding: utf-8 -*-

from itertools import izip
from math import fabs

def degree(poly):
    while poly and poly[-1] == 0:
        poly.pop()   # normalize
    return len(poly)-1

def poly_div(N, D):
    dD = degree(D)
    dN = degree(N)
    if dD < 0: raise ZeroDivisionError
    if dN >= dD:
        q = [0] * dN
        while dN >= dD:
            d = [0]*(dN - dD) + D
            mult = q[dN - dD] = N[-1] / float(d[-1])
            d = [coeff*mult for coeff in d]
            N = [fabs ( coeffN - coeffd ) for coeffN, coeffd in izip(N, d)]
            dN = degree(N)
        r = N
    else:
        q = [0]
        r = N
    return q, r

if __name__ == '__main__':
    print "POLYNOMIAL LONG DIVISION"
    N = [-42, 0, -12, 1]
    D = [-3, 1, 0, 0]
    print "  %s / %s =" % (N,D),
    print " %s remainder %s" % poly_div(N, D)
