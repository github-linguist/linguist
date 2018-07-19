"""
  Compute nearest pair of points using two algorithms

  First algorithm is 'brute force' comparison of every possible pair.
  Second, 'divide and conquer', is based on:
    www.cs.iupui.edu/~xkzou/teaching/CS580/Divide-and-conquer-closestPair.ppt
"""

from random import randint, randrange
from operator import itemgetter, attrgetter

infinity = float('inf')

# Note the use of complex numbers to represent 2D points making distance == abs(P1-P2)

def bruteForceClosestPair(point):
    numPoints = len(point)
    if numPoints < 2:
        return infinity, (None, None)
    return min( ((abs(point[i] - point[j]), (point[i], point[j]))
                 for i in range(numPoints-1)
                 for j in range(i+1,numPoints)),
                key=itemgetter(0))

def closestPair(point):
    xP = sorted(point, key= attrgetter('real'))
    yP = sorted(point, key= attrgetter('imag'))
    return _closestPair(xP, yP)

def _closestPair(xP, yP):
    numPoints = len(xP)
    if numPoints <= 3:
        return bruteForceClosestPair(xP)
    Pl = xP[:numPoints/2]
    Pr = xP[numPoints/2:]
    Yl, Yr = [], []
    xDivider = Pl[-1].real
    for p in yP:
        if p.real <= xDivider:
            Yl.append(p)
        else:
            Yr.append(p)
    dl, pairl = _closestPair(Pl, Yl)
    dr, pairr = _closestPair(Pr, Yr)
    dm, pairm = (dl, pairl) if dl < dr else (dr, pairr)
    # Points within dm of xDivider sorted by Y coord
    closeY = [p for p in yP  if abs(p.real - xDivider) < dm]
    numCloseY = len(closeY)
    if numCloseY > 1:
        # There is a proof that you only need compare a max of 7 next points
        closestY = min( ((abs(closeY[i] - closeY[j]), (closeY[i], closeY[j]))
                         for i in range(numCloseY-1)
                         for j in range(i+1,min(i+8, numCloseY))),
                        key=itemgetter(0))
        return (dm, pairm) if dm <= closestY[0] else closestY
    else:
        return dm, pairm

def times():
    ''' Time the different functions
    '''
    import timeit

    functions = [bruteForceClosestPair, closestPair]
    for f in functions:
        print 'Time for', f.__name__, timeit.Timer(
            '%s(pointList)' % f.__name__,
            'from closestpair import %s, pointList' % f.__name__).timeit(number=1)



pointList = [randint(0,1000)+1j*randint(0,1000) for i in range(2000)]

if __name__ == '__main__':
    pointList = [(5+9j), (9+3j), (2+0j), (8+4j), (7+4j), (9+10j), (1+9j), (8+2j), 10j, (9+6j)]
    print pointList
    print '  bruteForceClosestPair:', bruteForceClosestPair(pointList)
    print '            closestPair:', closestPair(pointList)
    for i in range(10):
        pointList = [randrange(11)+1j*randrange(11) for i in range(10)]
        print '\n', pointList
        print ' bruteForceClosestPair:', bruteForceClosestPair(pointList)
        print '           closestPair:', closestPair(pointList)
    print '\n'
    times()
    times()
    times()
