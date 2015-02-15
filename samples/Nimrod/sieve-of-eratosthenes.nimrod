import math, strutils

var is_prime: seq[Bool] = @[]
is_prime.add(False)
is_prime.add(False)

iterator iprimes_upto(limit: int): int =
    for n in high(is_prime) .. limit+2: is_prime.add(True)
    for n in 2 .. limit + 1:
        if is_prime[n]:
            yield n
            for i in countup((n *% n), limit+1, n): # start at ``n`` squared
                try:
                    is_prime[i] = False
                except EInvalidIndex: break


echo("Primes are:")
for x in iprimes_upto(200):
   write(stdout, x, " ")
writeln(stdout,"")
