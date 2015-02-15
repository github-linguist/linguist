from itertools import groupby, permutations

def A036058(number):
    return ''.join( str(len(list(g))) + k
                    for k,g in groupby(sorted(str(number), reverse=True)) )

def A036058_length(numberstring='0', printit=False):
    iterations, last_three, queue_index = 1, ([None] * 3), 0

    def A036058(number):
        # rely on external reverse-sort of digits of number
        return ''.join( str(len(list(g))) + k
                        for k,g in groupby(number) )

    while True:
        if printit:
            print("  %2i %s" % (iterations, numberstring))
        numberstring = ''.join(sorted(numberstring, reverse=True))
        if numberstring in last_three:
            break
        assert iterations < 1000000
        last_three[queue_index], numberstring = numberstring, A036058(numberstring)
        iterations += 1
        queue_index +=1
        queue_index %=3
    return iterations

def max_A036058_length( start_range=range(11) ):
    already_done = set()
    max_len = (-1, [])
    for n in start_range:
        sn = str(n)
        sns = tuple(sorted(sn, reverse=True))
        if sns not in already_done:
            already_done.add(sns)
            size = A036058_length(sns)
            if size > max_len[0]:
                max_len = (size, [n])
            elif size == max_len[0]:
                max_len[1].append(n)
    return max_len

lenmax, starts = max_A036058_length( range(1000000) )

# Expand
allstarts = []
for n in starts:
    allstarts += [int(''.join(x))
                  for x in set(k
                               for k in permutations(str(n), 4)
                               if k[0] != '0')]
allstarts = [x for x in sorted(allstarts) if x < 1000000]

print ( '''\
The longest length, followed by the number(s) with the longest sequence length
for starting sequence numbers below 1000000 are:
  Iterations = %i and sequence-starts = %s.''' % (lenmax, allstarts)   )

print ( '''
Note that only the first of any sequences with the same digits is printed below.
(The others will differ only in their first term)''' )

for n in starts:
    print()
    A036058_length(str(n), printit=True)
