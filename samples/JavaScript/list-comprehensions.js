function range(begin, end) {
    for (let i = begin; i < end; ++i)
        yield i;
}

function triples(n) {
    return [[x,y,z] for each (x in range(1,n+1))
                     for each (y in range(x,n+1))
                     for each (z in range(y,n+1))
                     if (x*x + y*y == z*z) ]
}

for each (var triple in triples(20))
    print(triple);
