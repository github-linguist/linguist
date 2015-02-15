from __future__ import division

def maxandmallows(nmaxpower2=20):
    # Note: The first hc number is returned in hc[1];
    # hc[0] is not part of the series.
    nmax = 2**nmaxpower2
    hc, mallows, mx, mxpow2 = [None, 1, 1], None, (0.5, 2), []
    for n in range(2, nmax + 1):
        ratio = hc[n] / n
        if ratio > mx[0]: mx = (ratio, n)
        if ratio >= 0.55: mallows = n
        if ratio == 0.5:
            print("In the region %7i < n <= %7i: max a(n)/n = %s" %
		  ((n)//2, n, "%6.4f at  n = %i" % mx))
            mxpow2.append(mx[0])
            mx = (ratio, n)
        hc.append(hc[hc[n]] + hc[-hc[n]])
    return hc, mallows if mxpow2 and mxpow2[-1] < 0.55 and n > 4 else None

if __name__ == '__main__':
    hc, mallows = maxandmallows(20)
    if mallows:
        print("\nYou too might have won $1000 with the mallows number of %i" % mallows)
