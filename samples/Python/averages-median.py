def median(aray):
    srtd = sorted(aray)
    alen = len(srtd)
    return 0.5*( srtd[(alen-1)//2] + srtd[alen//2])

a = (4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2)
print a, median(a)
a = (4.1, 7.2, 1.7, 9.3, 4.4, 3.2)
print a, median(a)
