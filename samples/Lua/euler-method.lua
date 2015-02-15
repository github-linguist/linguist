T0 = 100
TR = 20
k = 0.07
delta_t = { 2, 5, 10 }
n = 100

NewtonCooling = function( t ) return -k * ( t - TR ) end


function Euler( f, y0, n, h )
    local y = y0
    for x = 0, n, h do
	print( "", x, y )
 	y = y + h * f( y )
    end
end


for i = 1, #delta_t do
    print( "delta_t = ", delta_t[i] )
    Euler( NewtonCooling, T0, n, delta_t[i] )
end
