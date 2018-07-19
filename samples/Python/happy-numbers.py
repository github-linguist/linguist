>>> def happy(n):
    past = set()			
    while n != 1:
        n = sum(int(i)**2 for i in str(n))
        if n in past:
            return False
        past.add(n)
    return True

>>> [x for x in xrange(500) if happy(x)][:8]
[1, 7, 10, 13, 19, 23, 28, 31]
