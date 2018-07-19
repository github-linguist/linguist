'''
Forest-Fire Cellular automation
 See: http://en.wikipedia.org/wiki/Forest-fire_model
'''

L = 15
# d = 2 # Fixed
initial_trees = 0.55
p = 0.01
f = 0.001

try:
    raw_input
except:
    raw_input = input

import random


tree, burning, space = 'TB.'
hood = ((-1,-1), (-1,0), (-1,1),
        (0,-1),          (0, 1),
        (1,-1),  (1,0),  (1,1))

def initialise():
    grid = {(x,y): (tree if random.random()<= initial_trees else space)
            for x in range(L)
            for y in range(L) }
    return grid

def gprint(grid):
    txt = '\n'.join(''.join(grid[(x,y)] for x in range(L))
                    for y in range(L))
    print(txt)

def quickprint(grid):
    t = b = 0
    ll = L * L
    for x in range(L):
        for y in range(L):
            if grid[(x,y)] in (tree, burning):
                t += 1
                if grid[(x,y)] == burning:
                    b += 1
    print(('Of %6i cells, %6i are trees of which %6i are currently burning.'
          + ' (%6.3f%%, %6.3f%%)')
          % (ll, t, b, 100. * t / ll, 100. * b / ll))


def gnew(grid):
    newgrid = {}
    for x in range(L):
        for y in range(L):
            if grid[(x,y)] == burning:
                newgrid[(x,y)] = space
            elif grid[(x,y)] == space:
                newgrid[(x,y)] = tree if random.random()<= p else space
            elif grid[(x,y)] == tree:
                newgrid[(x,y)] = (burning
                                   if any(grid.get((x+dx,y+dy),space) == burning
                                            for dx,dy in hood)
                                        or random.random()<= f
                                   else tree)
    return newgrid

if __name__ == '__main__':
    grid = initialise()
    iter = 0
    while True:
        quickprint(grid)
        inp = raw_input('Print/Quit/<int>/<return> %6i: ' % iter).lower().strip()
        if inp:
            if inp[0] == 'p':
                gprint(grid)
            elif inp.isdigit():
                for i in range(int(inp)):
                    iter +=1
                    grid = gnew(grid)
                    quickprint(grid)
            elif inp[0] == 'q':
                break
        grid = gnew(grid)
        iter +=1
