def crossp(a, b):
    '''Cross product of two 3D vectors'''
    assert len(a) == len(b) == 3, 'For 3D vectors only'
    a1, a2, a3 = a
    b1, b2, b3 = b
    return (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

def dotp(a,b):
    '''Dot product of two eqi-dimensioned vectors'''
    assert len(a) == len(b), 'Vector sizes must match'
    return sum(aterm * bterm for aterm,bterm in zip(a, b))

def scalartriplep(a, b, c):
    '''Scalar triple product of three vectors: "a . (b x c)"'''
    return dotp(a, crossp(b, c))

def vectortriplep(a, b, c):
    '''Vector triple product of three vectors: "a x (b x c)"'''
    return crossp(a, crossp(b, c))

if __name__ == '__main__':
    a, b, c = (3, 4, 5), (4, 3, 5), (-5, -12, -13)
    print("a = %r;  b = %r;  c = %r" % (a, b, c))
    print("a . b =", dotp(a,b))
    print("a x b =", crossp(a,b))
    print("a . (b x c) =", scalartriplep(a, b, c))
    print("a x (b x c) =", vectortriplep(a, b, c))
