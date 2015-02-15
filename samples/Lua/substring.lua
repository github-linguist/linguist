str = "abcdefghijklmnopqrstuvwxyz"
n, m = 5, 15

print( string.sub( str, n, m ) )    -- efghijklmno
print( string.sub( str, n, -1 ) )   -- efghijklmnopqrstuvwxyz
print( string.sub( str, 1, -2 ) )   -- abcdefghijklmnopqrstuvwxy

pos = string.find( str, "i" )
if pos ~= nil then print( string.sub( str, pos, pos+m ) ) end -- ijklmnopqrstuvwx

pos = string.find( str, "ijk" )
if pos ~= nil then print( string.sub( str, pos, pos+m ) ) end-- ijklmnopqrstuvwx

-- Alternative (more modern) notation

print ( str:sub(n,m) )         -- efghijklmno
print ( str:sub(n) )           -- efghijklmnopqrstuvwxyz
print ( str:sub(1,-2) )        -- abcdefghijklmnopqrstuvwxy

pos = str:find "i"
if pos then print (str:sub(pos,pos+m)) end -- ijklmnopqrstuvwx

pos = str:find "ijk"
if pos then print (str:sub(pos,pos+m)) end d-- ijklmnopqrstuvwx
