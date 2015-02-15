from collections import Counter
from pprint import pprint as pp

def distcheck(fn, repeats, delta):
    '''\
    Bin the answers to fn() and check bin counts are within +/- delta %
    of repeats/bincount'''
    bin = Counter(fn() for i in range(repeats))
    target = repeats // len(bin)
    deltacount = int(delta / 100. * target)
    assert all( abs(target - count) < deltacount
                for count in bin.values() ), "Bin distribution skewed from %i +/- %i: %s" % (
                    target, deltacount, [ (key, target - count)
                                          for key, count in sorted(bin.items()) ]
                    )
    pp(dict(bin))
