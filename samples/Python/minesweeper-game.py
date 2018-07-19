'''
Minesweeper game.

    There is an n by m grid that has a random number of between 20% to 60%
    of randomly hidden mines that need to be found.

    Positions in the grid are modified by entering their coordinates
    where the first coordinate is horizontal in the grid and the second
    vertical. The top left of the grid is position 1,1; the bottom right is
    at n,m.

    * The total number of mines to be found is shown at the beginning of the
    game.
    * Each mine occupies a single grid point, and its position is initially
    unknown to the player
    * The grid is shown as a rectangle of characters between moves.
    * You are initially shown all grids as obscured, by a single dot '.'
    * You may mark what you think is the position of a mine which will show
    as a '?'
    * You can mark what you think is free space by entering its coordinates.
    :*  If the point is free space then it is cleared, as are any adjacent
    points that are also free space- this is repeated recursively for
    subsequent adjacent free points unless that point is marked as a mine
    or is a mine.
    ::*   Points marked as a mine show as a '?'.
    ::*   Other free points show as an integer count of the number of adjacent
    true mines in its immediate neighbourhood, or as a single space ' ' if the
    free point is not adjacent to any true mines.
    * Of course you loose if you try to clear space that starts on a mine.
    * You win when you have correctly identified all mines.


    When prompted you may:
        Toggle where you think a mine is at position x, y:
          m <x> <y>
        Clear the grid starting at position x, y (and print the result):
          c <x> <y>
        Print the grid so far:
          p
        Resign
          r
    Resigning will first show the grid with an 'N' for unfound true mines, a
    'Y' for found true mines and a '?' for where you marked clear space as a
    mine

'''


gridsize  = (6, 4)
minerange = (0.2, 0.6)


try:
    raw_input
except:
    raw_input = input

import random
from itertools import product
from pprint import pprint as pp


def gridandmines(gridsize=gridsize, minerange=minerange):
    xgrid, ygrid = gridsize
    minmines, maxmines = minerange
    minecount = xgrid * ygrid
    minecount = random.randint(int(minecount*minmines), int(minecount*maxmines))
    grid = set(product(range(xgrid), range(ygrid)))
    mines = set(random.sample(grid, minecount))
    show = {xy:'.' for xy in grid}
    return grid, mines, show

def printgrid(show, gridsize=gridsize):
    xgrid, ygrid = gridsize
    grid = '\n'.join(''.join(show[(x,y)] for x in range(xgrid))
                     for y in range(ygrid))
    print( grid )

def resign(showgrid, mines, markedmines):
    for m in mines:
        showgrid[m] = 'Y' if m in markedmines else 'N'

def clear(x,y, showgrid, grid, mines, markedmines):
    if showgrid[(x, y)] == '.':
        xychar = str(sum(1
                         for xx in (x-1, x, x+1)
                         for yy in (y-1, y, y+1)
                         if (xx, yy) in mines ))
        if xychar == '0': xychar = '.'
        showgrid[(x,y)] = xychar
        for xx in (x-1, x, x+1):
            for yy in (y-1, y, y+1):
                xxyy = (xx, yy)
                if ( xxyy != (x, y)
                     and xxyy in grid
                     and xxyy not in mines | markedmines ):
                    clear(xx, yy, showgrid, grid, mines, markedmines)

if __name__ == '__main__':
    grid, mines, showgrid = gridandmines()
    markedmines = set([])
    print( __doc__ )
    print( '\nThere are %i true mines of fixed position in the grid\n' % len(mines) )
    printgrid(showgrid)
    while markedmines != mines:
        inp = raw_input('m x y/c x y/p/r: ').strip().split()
        if inp:
            if inp[0] == 'm':
                x, y = [int(i)-1 for i in inp[1:3]]
                if (x,y) in markedmines:
                    markedmines.remove((x,y))
                    showgrid[(x,y)] = '.'
                else:
                    markedmines.add((x,y))
                    showgrid[(x,y)] = '?'
            elif inp[0] == 'p':
                printgrid(showgrid)
            elif inp[0] == 'c':
                x, y = [int(i)-1 for i in inp[1:3]]
                if (x,y) in mines | markedmines:
                    print( '\nKLABOOM!! You hit a mine.\n' )
                    resign(showgrid, mines, markedmines)
                    printgrid(showgrid)
                    break
                clear(x,y, showgrid, grid, mines, markedmines)
                printgrid(showgrid)
            elif inp[0] == 'r':
                print( '\nResigning!\n' )
                resign(showgrid, mines, markedmines)
                printgrid(showgrid)
                break

    print( '\nYou got %i and missed %i of the %i mines'
           % (len(mines.intersection(markedmines)),
              len(markedmines.difference(mines)),
              len(mines)) )
