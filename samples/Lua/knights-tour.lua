N = 8

moves = { {1,-2},{2,-1},{2,1},{1,2},{-1,2},{-2,1},{-2,-1},{-1,-2} }

function Move_Allowed( board, x, y )
    if board[x][y] >= 8 then return false end

    local new_x, new_y = x + moves[board[x][y]+1][1], y + moves[board[x][y]+1][2]
    if new_x >= 1 and new_x <= N and new_y >= 1 and new_y <= N and board[new_x][new_y] == 0 then return true end

    return false
end

		
board = {}
for i = 1, N do
    board[i] = {}
    for j = 1, N do
        board[i][j] = 0
    end
end

x, y = 1, 1

lst = {}
lst[1] = { x, y }

repeat
    if Move_Allowed( board, x, y ) then
        board[x][y] = board[x][y] + 1
        x, y = x+moves[board[x][y]][1], y+moves[board[x][y]][2]
        lst[#lst+1] = { x, y }
    else
        if board[x][y] >= 8 then
            board[x][y] = 0
            lst[#lst] = nil
            if #lst == 0 then
                print "No solution found."
                os.exit(1)
            end
            x, y = lst[#lst][1], lst[#lst][2]
        end
        board[x][y] = board[x][y] + 1
    end
until #lst == N^2

last = lst[1]
for i = 2, #lst do
    print( string.format( "%s%d - %s%d", string.sub("ABCDEFGH",last[1],last[1]), last[2], string.sub("ABCDEFGH",lst[i][1],lst[i][1]), lst[i][2] ) )
    last = lst[i]
end
