from itertools import permutations
from operator import mul
from math import fsum
from spermutations import spermutations

def prod(lst):
    return reduce(mul, lst, 1)

def perm(a):
    n = len(a)
    r = range(n)
    s = permutations(r)
    return fsum(prod(a[i][sigma[i]] for i in r) for sigma in s)

def det(a):
    n = len(a)
    r = range(n)
    s = spermutations(n)
    return fsum(sign * prod(a[i][sigma[i]] for i in r)
                for sigma, sign in s)

if __name__ == '__main__':
    from pprint import pprint as pp

    for a in (
            [
             [1, 2],
             [3, 4]],

            [
             [1, 2, 3, 4],
             [4, 5, 6, 7],
             [7, 8, 9, 10],
             [10, 11, 12, 13]],

            [
             [ 0,  1,  2,  3,  4],
             [ 5,  6,  7,  8,  9],
             [10, 11, 12, 13, 14],
             [15, 16, 17, 18, 19],
             [20, 21, 22, 23, 24]],
        ):
        print('')
        pp(a)
        print('Perm: %s Det: %s' % (perm(a), det(a)))
