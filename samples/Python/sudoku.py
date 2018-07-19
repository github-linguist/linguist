def initiate():
    box.append([0, 1, 2, 9, 10, 11, 18, 19, 20])
    box.append([3, 4, 5, 12, 13, 14, 21, 22, 23])
    box.append([6, 7, 8, 15, 16, 17, 24, 25, 26])
    box.append([27, 28, 29, 36, 37, 38, 45, 46, 47])
    box.append([30, 31, 32, 39, 40, 41, 48, 49, 50])
    box.append([33, 34, 35, 42, 43, 44, 51, 52, 53])
    box.append([54, 55, 56, 63, 64, 65, 72, 73, 74])
    box.append([57, 58, 59, 66, 67, 68, 75, 76, 77])
    box.append([60, 61, 62, 69, 70, 71, 78, 79, 80])
    for i in range(0, 81, 9):
        row.append(range(i, i+9))
    for i in range(9):
        column.append(range(i, 80+i, 9))

def valid(n, pos):
    current_row = pos/9
    current_col = pos%9
    current_box = (current_row/3)*3 + (current_col/3)
    for i in row[current_row]:
        if (grid[i] == n):
            return False
    for i in column[current_col]:
        if (grid[i] == n):
            return False
    for i in box[current_box]:
        if (grid[i] == n):
            return False
    return True

def solve():
    i = 0
    proceed = 1
    while(i < 81):
        if given[i]:
            if proceed:
                    i += 1
            else:
                i -= 1
        else:
            n = grid[i]
            prev = grid[i]
            while(n < 9):
              if (n < 9):
                  n += 1
              if valid(n, i):
                  grid[i] = n
                  proceed = 1
                  break
            if (grid[i] == prev):
               grid[i] = 0
               proceed = 0
            if proceed:
               i += 1
            else:
               i -=1

def inputs():
    nextt = 'T'
    number = 0
    pos = 0
    while(not(nextt == 'N' or nextt == 'n')):
        print "Enter the position:",
        pos = int(raw_input())
        given[pos - 1] = True
        print "Enter the numerical:",
        number = int(raw_input())
        grid[pos - 1] = number
        print "Do you want to enter another given?(Y, for yes: N, for no)"
        nextt = raw_input()


grid = [0]*81
given = [False]*81
box = []
row = []
column = []
initiate()
inputs()
solve()
for i in range(9):
    print grid[i*9:i*9+9]
raw_input()
