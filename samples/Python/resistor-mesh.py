DIFF_THRESHOLD = 1e-40

class Fixed:
    FREE = 0
    A = 1
    B = 2

class Node:
    __slots__ = ["voltage", "fixed"]
    def __init__(self, v=0.0, f=Fixed.FREE):
        self.voltage = v
        self.fixed = f

def set_boundary(m):
    m[1][1] = Node( 1.0, Fixed.A)
    m[6][7] = Node(-1.0, Fixed.B)

def calc_difference(m, d):
    h = len(m)
    w = len(m[0])
    total = 0.0

    for i in xrange(h):
        for j in xrange(w):
            v = 0.0
            n = 0
            if i != 0:  v += m[i-1][j].voltage; n += 1
            if j != 0:  v += m[i][j-1].voltage; n += 1
            if i < h-1: v += m[i+1][j].voltage; n += 1
            if j < w-1: v += m[i][j+1].voltage; n += 1
            v = m[i][j].voltage - v / n

            d[i][j].voltage = v
            if m[i][j].fixed == Fixed.FREE:
                total += v ** 2
    return total

def iter(m):
    h = len(m)
    w = len(m[0])
    difference = [[Node() for j in xrange(w)] for i in xrange(h)]

    while True:
        set_boundary(m) # Enforce boundary conditions.
        if calc_difference(m, difference) < DIFF_THRESHOLD:
            break
        for i, di in enumerate(difference):
            for j, dij in enumerate(di):
                m[i][j].voltage -= dij.voltage

    cur = [0.0] * 3
    for i, di in enumerate(difference):
        for j, dij in enumerate(di):
            cur[m[i][j].fixed] += (dij.voltage *
                (bool(i) + bool(j) + (i < h-1) + (j < w-1)))

    return (cur[Fixed.A] - cur[Fixed.B]) / 2.0

def main():
    w = h = 10
    mesh = [[Node() for j in xrange(w)] for i in xrange(h)]
    print "R = %.16f" % (2 / iter(mesh))

main()
