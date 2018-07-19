def _pr(t, x, y, z):
    txt = '\n'.join(''.join(t[(n,m)] for n in range(3+x+z)).rstrip()
                    for m in reversed(range(3+y+z)))
    return txt
		
def cuboid(x,y,z):
    t = {(n,m):' ' for n in range(3+x+z) for m in range(3+y+z)}
    xrow = ['+'] + ['%i' % (i % 10) for i in range(x)] + ['+']
    for i,ch in enumerate(xrow):
        t[(i,0)] = t[(i,1+y)] = t[(1+z+i,2+y+z)] = ch
    if _debug: print(_pr(t, x, y, z))
    ycol = ['+'] + ['%i' % (j % 10) for j in range(y)] + ['+']
    for j,ch in enumerate(ycol):
        t[(0,j)] = t[(x+1,j)] = t[(2+x+z,1+z+j)] = ch
    zdepth = ['+'] + ['%i' % (k % 10) for k in range(z)] + ['+']
    if _debug: print(_pr(t, x, y, z))
    for k,ch in enumerate(zdepth):
        t[(k,1+y+k)] = t[(1+x+k,1+y+k)] = t[(1+x+k,k)] = ch
	
    return _pr(t, x, y, z)


_debug = False
if __name__ == '__main__':
    for dim in ((2,3,4), (3,4,2), (4,2,3)):
        print("CUBOID%r" % (dim,), cuboid(*dim), sep='\n')
