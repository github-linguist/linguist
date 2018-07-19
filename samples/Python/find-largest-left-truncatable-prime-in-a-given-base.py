import random

def is_probable_prime(n,k):
    #this uses the miller-rabin primality test found from rosetta code
    if n==0 or n==1:
        return False
    if n==2:
        return True
    if n % 2 == 0:
        return False
    s = 0
    d = n-1

    while True:
        quotient, remainder = divmod(d, 2)
        if remainder == 1:
            break
        s += 1
        d = quotient

    def try_composite(a):
        if pow(a, d, n) == 1:
            return False
        for i in range(s):
            if pow(a, 2**i * d, n) == n-1:
                return False
        return True # n is definitely composite

    for i in range(k):
        a = random.randrange(2, n)
        if try_composite(a):
            return False

    return True # no base tested showed n as composite


def largest_left_truncatable_prime(base):
    radix = 0
    candidates = [0]
    while True:
        new_candidates=[]
        multiplier = base**radix
        for i in range(1,base):
            new_candidates += [x+i*multiplier for x in candidates if is_probable_prime(x+i*multiplier,30)]
        if len(new_candidates)==0:
            return max(candidates)
        candidates = new_candidates
        radix += 1

for b in range(3,24):
    print("%d:%d\n" % (b,largest_left_truncatable_prime(b)))
