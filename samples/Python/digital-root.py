def droot (n):
    x = [n]
    while x[-1] > 10:
        x.append(sum(int(dig) for dig in str(x[-1])))
    return len(x) - 1, x[-1]

for n in [627615, 39390, 588225, 393900588225]:
    a, d = droot (n)
    print "%12i has additive persistance %2i and digital root of %i" % (
        n, a, d)
