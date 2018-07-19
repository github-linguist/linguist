from functools import lru_cache

primes = [2, 3, 5, 7, 11, 13, 17]    # Will be extended

@lru_cache(maxsize=2000)
def pfactor(n):
    if n == 1:
        return [1]
    n2 = n // 2 + 1
    for p in primes:
        if p <= n2:
            d, m = divmod(n, p)
            if m == 0:
                if d > 1:
                    return [p] + pfactor(d)
                else:
                    return [p]
        else:
            if n > primes[-1]:
                primes.append(n)
            return [n]

if __name__ == '__main__':
    mx = 5000
    for n in range(1, mx + 1):
        factors = pfactor(n)
        if n <= 10 or n >= mx - 20:
            print( '%4i %5s %s' % (n,
                                   '' if factors != [n] or n == 1 else 'prime',
                                   'x'.join(str(i) for i in factors)) )
        if n == 11:
            print('...')

    print('\nNumber of primes gathered up to', n, 'is', len(primes))
    print(pfactor.cache_info())
