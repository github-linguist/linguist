function hailstone( n, print_numbers )
    local n_iter = 1

    while n ~= 1 do
        if print_numbers then print( n ) end
        if n % 2 == 0 then
            n = n / 2
        else
            n = 3 * n + 1
        end

        n_iter = n_iter + 1
    end
    if print_numbers then print( n ) end

    return n_iter;
end

hailstone( 27, true )

max_i, max_iter = 0, 0
for i = 1, 100000 do
    num = hailstone( i, false )
    if num >= max_iter then
        max_i = i
        max_iter = num
    end
end

print( string.format( "Needed %d iterations for the number %d.\n", max_iter, max_i ) )
