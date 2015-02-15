N = 8

board = {}
for i = 1, N do
    board[i] = {}
    for j = 1, N do
	board[i][j] = false
    end
end

function Allowed( x, y )
    for i = 1, x-1 do
	if ( board[i][y] ) or ( i <= y and board[x-i][y-i] ) or ( y+i <= N and board[x-i][y+i] ) then
  	    return false
	end
    end		
    return true
end

function Find_Solution( x )
    for y = 1, N do
	if Allowed( x, y ) then
  	    board[x][y] = true
	    if x == N or Find_Solution( x+1 ) then
		return true
	    end
	    board[x][y] = false			
	end		
    end
    return false
end
		
if Find_Solution( 1 ) then
    for i = 1, N do
 	for j = 1, N do
  	    if board[i][j] then
		io.write( "|Q" )
	    else
		io.write( "| " )
	    end
	end
	print( "|" )
    end
else
    print( string.format( "No solution for %d queens.\n", N ) )
end
