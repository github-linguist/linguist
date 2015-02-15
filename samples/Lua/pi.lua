a = {}
n = 1000
len = math.modf( 10 * n / 3 )

for j = 1, len do
    a[j] = 2
end
nines = 0
predigit = 0
for j = 1, n do
    q = 0
    for i = len, 1, -1 do
        x = 10 * a[i] + q * i
        a[i] = math.fmod( x, 2 * i - 1 )
        q = math.modf( x / ( 2 * i - 1 ) )
    end
    a[1] = math.fmod( q, 10 )
    q = math.modf( q / 10 )
    if q == 9 then
        nines = nines + 1
    else
        if q == 10 then
            io.write( predigit + 1 )
            for k = 1, nines do
                io.write(0)
            end
            predigit = 0
            nines = 0
        else
            io.write( predigit )
            predigit = q
            if nines ~= 0 then
                for k = 1, nines do
                    io.write( 9 )
                end
                nines = 0
            end
        end
    end
end
print( predigit )
