class Isprime():
    '''
    Extensible sieve of Erastosthenes

    >>> isprime.check(11)
    True
    >>> isprime.multiples
    {2, 4, 6, 8, 9, 10}
    >>> isprime.primes
    [2, 3, 5, 7, 11]
    >>> isprime(13)
    True
    >>> isprime.multiples
    {2, 4, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 18, 20, 21, 22}
    >>> isprime.primes
    [2, 3, 5, 7, 11, 13, 17, 19]
    >>> isprime.nmax
    22
    >>>
    '''
    multiples = {2}
    primes = [2]
    nmax = 2

    def __init__(self, nmax):
        if nmax > self.nmax:
            self.check(nmax)

    def check(self, n):
        if type(n) == float:
            if not n.is_integer(): return False
            n = int(n)
        multiples = self.multiples
        if n <= self.nmax:
            return n not in multiples
        else:
            # Extend the sieve
            primes, nmax = self.primes, self.nmax
            newmax = max(nmax*2, n)
            for p in primes:
                multiples.update(range(p*((nmax + p + 1) // p), newmax+1, p))
            for i in range(nmax+1, newmax+1):
                if i not in multiples:
                    primes.append(i)
                    multiples.update(range(i*2, newmax+1, i))
            self.nmax = newmax
            return n not in multiples

    __call__ = check


def carmichael(p1):
    ans = []
    if isprime(p1):
        for h3 in range(2, p1):
            g = h3 + p1
            for d in range(1, g):
                if (g * (p1 - 1)) % d == 0 and (-p1 * p1) % h3 == d % h3:
                    p2 = 1 + ((p1 - 1)* g // d)
                    if isprime(p2):
                        p3 = 1 + (p1 * p2 // h3)
                        if isprime(p3):
                            if (p2 * p3) % (p1 - 1) == 1:
                                #print('%i X %i X %i' % (p1, p2, p3))
                                ans += [tuple(sorted((p1, p2, p3)))]
    return ans

isprime = Isprime(2)

ans = sorted(sum((carmichael(n) for n in range(62) if isprime(n)), []))
print(',\n'.join(repr(ans[i:i+5])[1:-1] for i in range(0, len(ans)+1, 5)))
