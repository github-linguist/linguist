from random import randrange
try:
    range = xrange
except: pass

def one_of_n(lines): # lines is any iterable
    choice = None
    for i, line in enumerate(lines):
        if randrange(i+1) == 0:
            choice = line
    return choice

def one_of_n_test(n=10, trials=1000000):
    bins = [0] * n
    if n:
        for i in range(trials):
            bins[one_of_n(range(n))] += 1
    return bins

print(one_of_n_test())
