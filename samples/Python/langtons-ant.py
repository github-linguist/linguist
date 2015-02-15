width = 75
height = 52
nsteps = 12000

class Dir: up, right, down, left = range(4)
class Turn: left, right = False, True
class Color: white, black = '.', '#'
M = [[Color.white] * width for _ in xrange(height)]

x = width // 2
y = height // 2
dir = Dir.up

i = 0
while i < nsteps and 0 <= x < width and 0 <= y < height:
    turn = Turn.left if M[y][x] == Color.black else Turn.right
    M[y][x] = Color.white if M[y][x] == Color.black else Color.black

    dir = (4 + dir + (1 if turn else -1)) % 4
    dir = [Dir.up, Dir.right, Dir.down, Dir.left][dir]
    if   dir == Dir.up:    y -= 1
    elif dir == Dir.right: x -= 1
    elif dir == Dir.down:  y += 1
    elif dir == Dir.left:  x += 1
    else: assert False
    i += 1

print "\n".join("".join(row) for row in M)
