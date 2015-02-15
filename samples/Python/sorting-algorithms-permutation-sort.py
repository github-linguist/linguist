from itertools import permutations

in_order = lambda s: all(x <= s[i+1] for i,x in enumerate(s[:-1]))
perm_sort = lambda s: (p for p in permutations(s) if in_order(p)).next()
