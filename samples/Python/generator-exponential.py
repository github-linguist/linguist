from itertools import islice, count

def powers(m):
    for n in count():
        yield n ** m

def filtered(s1, s2):
    v, f = next(s1), next(s2)
    while True:
        if v > f:
            f = next(s2)
            continue
        elif v < f:
            yield v
        v = next(s1)

squares, cubes = powers(2), powers(3)
f = filtered(squares, cubes)
print(list(islice(f, 20, 30)))
