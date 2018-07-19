def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def lcm(a, b):
    return (a*b) / gcd(a, b)

def isPrime(p):
    return (p > 1) and all(f == p for f,e in factored(p))

primeList = [2,3,5,7]
def primes():
    for p in primeList:
        yield p
    while 1:
        p += 2
        while not isPrime(p):
            p += 2
        primeList.append(p)
        yield p

def factored( a):
    for p in primes():
        j = 0
        while a%p == 0:
            a /= p
            j += 1
        if j > 0:
            yield (p,j)
        if a < p*p: break
    if a > 1:
        yield (a,1)


def multOrdr1(a,(p,e) ):
    m = p**e
    t = (p-1)*(p**(e-1)) #  = Phi(p**e) where p prime
    qs = [1,]
    for f in factored(t):
        qs = [ q * f[0]**j for j in range(1+f[1]) for q in qs ]
    qs.sort()

    for q in qs:
        if pow( a, q, m )==1: break
    return q


def multOrder(a,m):
    assert gcd(a,m) == 1
    mofs = (multOrdr1(a,r) for r in factored(m))
    return reduce(lcm, mofs, 1)


if __name__ == "__main__":
    print multOrder(37, 1000)        # 100
    b = 10**20-1
    print multOrder(2, b) # 3748806900
    print multOrder(17,b) # 1499522760
    b = 100001
    print multOrder(54,b)
    print pow( 54, multOrder(54,b),b)
    if any( (1==pow(54,r, b)) for r in range(1,multOrder(54,b))):
        print 'Exists a power r < 9090 where pow(54,r,b)==1'
    else:
        print 'Everything checks.'
