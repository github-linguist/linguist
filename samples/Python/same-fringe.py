try:
    from itertools import zip_longest as izip_longest # Python 3.x
except:
    from itertools import izip_longest                # Python 2.6+

def fringe(tree):
    """Yield tree members L-to-R depth first,
    as if stored in a binary tree"""
    for node1 in tree:
        if isinstance(node1, tuple):
            for node2 in fringe(node1):
                yield node2
        else:
            yield node1

def same_fringe(tree1, tree2):
    return all(node1 == node2 for node1, node2 in
               izip_longest(fringe(tree1), fringe(tree2)))

if __name__ == '__main__':
    a = 1, 2, 3, 4, 5, 6, 7, 8
    b = 1, (( 2, 3 ), (4, (5, ((6, 7), 8))))
    c = (((1, 2), 3), 4), 5, 6, 7, 8

    x = 1, 2, 3, 4, 5, 6, 7, 8, 9
    y = 0, 2, 3, 4, 5, 6, 7, 8
    z = 1, 2, (4, 3), 5, 6, 7, 8

    assert same_fringe(a, a)
    assert same_fringe(a, b)
    assert same_fringe(a, c)

    assert not same_fringe(a, x)
    assert not same_fringe(a, y)
    assert not same_fringe(a, z)
