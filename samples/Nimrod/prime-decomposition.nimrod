import strutils, math, sequtils, times

proc getStep(n: int64) : int64 {.inline.} =
   result = 1 + n*4 - int64(n /% 2)*2

proc primeFac(n: int64): seq[int64] =
    var res: seq[int64] = @[]
    var maxq = int64(floor(sqrt(float(n))))
    var d = 1
    var q: int64 = (n %% 2) and 2 or 3    # either 2 or 3, alternating
    while (q <= maxq) and ((n %% q) != 0):
        q = getStep(d)
        d += 1
    if q <= maxq:
        var q1: seq[int64] = primeFac(n /% q)
        var q2: seq[int64] = primeFac(q)
        res = concat(q2, q1, res)
    else:
        res.add(n)
    result = res

var is_prime: seq[Bool] = @[]
is_prime.add(False)
is_prime.add(False)

iterator primes(limit: int): int =
    for n in high(is_prime) .. limit+2: is_prime.add(True)
    for n in 2 .. limit + 1:
        if is_prime[n]:
            yield n
            for i in countup((n *% n), limit+1, n): # start at ``n`` squared
                try:
                    is_prime[i] = False
                except EInvalidIndex: break

# Example: calculate factors of Mersenne numbers to M59 #

for m in primes(59):
    var p = int64(pow(2.0,float(m)) - 1)
    write(stdout,"2**$1-1 = $2, with factors: " % [$m, $p] )
    var start = cpuTime()
    var f = primeFac(p)
    for factor in f:
        write(stdout, factor)
        write(stdout, ", ")
        FlushFile(stdout)
    writeln(stdout, "=> $#ms" % $int(1000*(cpuTime()-start)) )
