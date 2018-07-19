def range_extract(lst):
    'Yield 2-tuple ranges or 1-tuple single elements from list of increasing ints'
    lenlst = len(lst)
    i = 0
    while i< lenlst:
        low = lst[i]
        while i <lenlst-1 and lst[i]+1 == lst[i+1]: i +=1
        hi = lst[i]
        if   hi - low >= 2:
            yield (low, hi)
        elif hi - low == 1:
            yield (low,)
            yield (hi,)
        else:
            yield (low,)
        i += 1

def printr(ranges):
    print( ','.join( (('%i-%i' % r) if len(r) == 2 else '%i' % r)
                     for r in ranges ) )

if __name__ == '__main__':
    for lst in [[-8, -7, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7,
                 8, 9, 10, 11, 14, 15, 17, 18, 19, 20],
                [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39]]:
        #print(list(range_extract(lst)))
        printr(range_extract(lst))
