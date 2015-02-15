import math
import random

def GammaInc_Q( a, x):
    a1 = a-1
    a2 = a-2
    def f0( t ):
        return t**a1*math.exp(-t)

    def df0(t):
        return (a1-t)*t**a2*math.exp(-t)

    y = a1
    while f0(y)*(x-y) >2.0e-8 and y < x: y += .3
    if y > x: y = x

    h = 3.0e-4
    n = int(y/h)
    h = y/n
    hh = 0.5*h
    gamax = h * sum( f0(t)+hh*df0(t) for t in ( h*j for j in xrange(n-1, -1, -1)))

    return gamax/gamma_spounge(a)

c = None
def gamma_spounge( z):
    global c
    a = 12

    if c is None:
       k1_factrl = 1.0
       c = []
       c.append(math.sqrt(2.0*math.pi))
       for k in range(1,a):
          c.append( math.exp(a-k) * (a-k)**(k-0.5) / k1_factrl )
          k1_factrl *= -k

    accm = c[0]
    for k in range(1,a):
        accm += c[k] / (z+k)
    accm *= math.exp( -(z+a)) * (z+a)**(z+0.5)
    return accm/z;

def chi2UniformDistance( dataSet ):
    expected = sum(dataSet)*1.0/len(dataSet)
    cntrd = (d-expected for d in dataSet)
    return sum(x*x for x in cntrd)/expected

def chi2Probability(dof, distance):
    return 1.0 - GammaInc_Q( 0.5*dof, 0.5*distance)

def chi2IsUniform(dataSet, significance):
    dof = len(dataSet)-1
    dist = chi2UniformDistance(dataSet)
    return chi2Probability( dof, dist ) > significance

dset1 = [ 199809, 200665, 199607, 200270, 199649 ]
dset2 = [ 522573, 244456, 139979,  71531,  21461 ]

for ds in (dset1, dset2):
    print "Data set:", ds
    dof = len(ds)-1
    distance =chi2UniformDistance(ds)
    print "dof: %d distance: %.4f" % (dof, distance),
    prob = chi2Probability( dof, distance)
    print "probability: %.4f"%prob,
    print "uniform? ", "Yes"if chi2IsUniform(ds,0.05) else "No"
