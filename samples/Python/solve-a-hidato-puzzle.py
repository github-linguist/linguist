board = []
given = []
start = None

def setup(s):
    global board, given, start
    lines = s.splitlines()
    ncols = len(lines[0].split())
    nrows = len(lines)
    board = [[-1] * (ncols + 2) for _ in xrange(nrows + 2)]

    for r, row in enumerate(lines):
        for c, cell in enumerate(row.split()):
            if cell == "__" :
                board[r + 1][c + 1] = 0
                continue
            elif cell == ".":
                continue # -1
            else:
                val = int(cell)
                board[r + 1][c + 1] = val
                given.append(val)
                if val == 1:
                    start = (r + 1, c + 1)
    given.sort()

def solve(r, c, n, next=0):
    if n > given[-1]:
        return True
    if board[r][c] and board[r][c] != n:
        return False
    if board[r][c] == 0 and given[next] == n:
        return False

    back = 0
    if board[r][c] == n:
        next += 1
        back = n

    board[r][c] = n
    for i in xrange(-1, 2):
        for j in xrange(-1, 2):
            if solve(r + i, c + j, n + 1, next):
                return True
    board[r][c] = back
    return False

def print_board():
    d = {-1: "  ", 0: "__"}
    bmax = max(max(r) for r in board)
    form = "%" + str(len(str(bmax)) + 1) + "s"
    for r in board[1:-1]:
        print "".join(form % d.get(c, str(c)) for c in r[1:-1])

hi = """\
__ 33 35 __ __  .  .  .
__ __ 24 22 __  .  .  .
__ __ __ 21 __ __  .  .
__ 26 __ 13 40 11  .  .
27 __ __ __  9 __  1  .
 .  . __ __ 18 __ __  .
 .  .  .  . __  7 __ __
 .  .  .  .  .  .  5 __"""

setup(hi)
print_board()
solve(start[0], start[1], 1)
print
print_board()
