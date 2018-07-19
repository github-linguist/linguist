filename = "file.txt"

x = { 1, 2, 3, 1e11 }
y = { 1, 1.4142135623730951, 1.7320508075688772, 316227.76601683791 };
xprecision = 3;
yprecision = 5;

fstr = "%."..tostring(xprecision).."f ".."%."..tostring(yprecision).."f\n"

fp = io.open( filename, "w+" )

for i = 1, #x do
    fp:write( string.format( fstr, x[i], y[i] ) )
end

io.close( fp )
