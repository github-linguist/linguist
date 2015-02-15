import strutils, algorithm

var board: array[0..19, array[0..19, int]]
var given, start: seq[int] = @[]
var rows, cols: int = 0

proc setup(s: string) =
    var lines = s.splitLines()
    cols = lines[0].split().len()
    rows = lines.len()

    for i in 0 .. rows + 1:
        for j in 0 .. cols + 1:
           board[i][j] = -1

    for r, row in pairs(lines):
        for c, cell in pairs(row.split()):
            case cell
            of "__" :
                board[r + 1][c + 1] = 0
                continue
            of "." : continue
            else :
               var val = parseInt(cell)
               board[r + 1][c + 1] = val
               given.add(val)
               if (val == 1):
                   start.add(r + 1)
                   start.add(c + 1)
    given.sort(cmp[int], Ascending)

proc solve(r, c, n: int, next: int = 0): Bool =
    if n > given[high(given)]:
       return True
    if board[r][c] < 0:
        return False
    if (board[r][c] > 0 and board[r][c] != n):
        return False
    if (board[r][c] == 0 and given[next] == n):
        return False

    var back = board[r][c]
    board[r][c] = n
    for i in -1 .. 1:
        for j in -1 .. 1:
            if back == n:
                if (solve(r + i, c + j, n + 1, next + 1)):  return True
            else:
                if (solve(r + i, c + j, n + 1, next)): return True
    board[r][c] = back
    result = False


proc printBoard() =
    for r in  0 .. rows + 1:
        for cellid,c in pairs(board[r]):
            if cellid > cols + 1: break
            if c == -1:
                write(stdout, " . ")
            elif c == 0:
                write(stdout, "__ ")
            else:
                write(stdout, "$# " % align($c,2))
        writeln(stdout, "")

var hi: string = """__ 33 35 __ __  .  .  .
__ __ 24 22 __  .  .  .
__ __ __ 21 __ __  .  .
__ 26 __ 13 40 11  .  .
27 __ __ __  9 __  1  .
.  . __ __ 18 __ __  .
.  .  .  . __  7 __ __
.  .  .  .  .  .  5 __"""

setup(hi)
printBoard()
echo("")
echo("Found:")
discard solve(start[0], start[1], 1)
printBoard()
