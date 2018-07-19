'''
Wireworld implementation.
'''

from io import StringIO
from collections import namedtuple
from pprint import pprint as pp
import copy

WW = namedtuple('WW', 'world, w, h')
head, tail, conductor, empty = allstates = 'Ht. '


infile = StringIO('''\
tH.........
.   .
   ...
.   .
Ht.. ......\
''')

def readfile(f):
    '''file > initial world configuration'''
    world  = [row.rstrip('\r\n') for row in f]
    height = len(world)
    width  = max(len(row) for row in world)
    # fill right and frame in empty cells
    nonrow = [ " %*s " % (-width, "") ]
    world  = nonrow + \
               [ " %*s " % (-width, row) for row in world ] + \
               nonrow
    world = [list(row) for row in world]
    return WW(world, width, height)

def newcell(currentworld, x, y):
    istate = currentworld[y][x]
    assert istate in allstates, 'Wireworld cell set to unknown value "%s"' % istate
    if istate == head:
        ostate = tail
    elif istate == tail:
        ostate = conductor
    elif istate == empty:
        ostate = empty
    else: # istate == conductor
        n = sum( currentworld[y+dy][x+dx] == head
                 for dx,dy in ( (-1,-1), (-1,+0), (-1,+1),
                                (+0,-1),          (+0,+1),
                                (+1,-1), (+1,+0), (+1,+1) ) )
        ostate = head if 1 <= n <= 2 else conductor
    return ostate

def nextgen(ww):
    'compute next generation of wireworld'
    world, width, height = ww
    newworld = copy.deepcopy(world)
    for x in range(1, width+1):
        for y in range(1, height+1):
            newworld[y][x] = newcell(world, x, y)
    return WW(newworld, width, height)

def world2string(ww):
    return '\n'.join( ''.join(row[1:-1]).rstrip() for row in ww.world[1:-1] )

ww = readfile(infile)
infile.close()

for gen in range(10):
    print ( ("\n%3i " % gen) + '=' * (ww.w-4) + '\n' )
    print ( world2string(ww) )
    ww = nextgen(ww)
