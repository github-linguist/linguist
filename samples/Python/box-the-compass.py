majors   = 'north east south west'.split()
majors   *= 2 # no need for modulo later
quarter1 = 'N,N by E,N-NE,NE by N,NE,NE by E,E-NE,E by N'.split(',')
quarter2 = [p.replace('NE','EN') for p in quarter1]

def degrees2compasspoint(d):
    d = (d % 360) + 360/64
    majorindex, minor = divmod(d, 90.)
    majorindex = int(majorindex)
    minorindex  = int( (minor*4) // 45 )
    p1, p2 = majors[majorindex: majorindex+2]
    if p1 in {'north', 'south'}:
        q = quarter1
    else:
        q = quarter2
    return q[minorindex].replace('N', p1).replace('E', p2).capitalize()

if __name__ == '__main__':
    for i in range(33):
        d = i * 11.25
        m = i % 3
        if   m == 1: d += 5.62
        elif m == 2: d -= 5.62
        n = i % 32 + 1
        print( '%2i %-18s %7.2f°' % (n, degrees2compasspoint(d), d) )
