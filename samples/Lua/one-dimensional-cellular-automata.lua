num_iterations = 9
f = { 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0 }

function Output( f, l )
    io.write( l, ":  " )
    for i = 1, #f do
        local c
        if f[i] == 1 then c = '#' else c = '_' end
        io.write( c )
    end
    print ""
end

Output( f, 0 )

for l = 1, num_iterations do
    local g = {}
    for i = 2, #f-1 do
        if f[i-1] + f[i+1] == 1 then
            g[i] = f[i]
        elseif f[i] == 0 and f[i-1] + f[i+1] == 2 then
            g[i] = 1
        else
            g[i] = 0
        end
    end
    if f[1]  == 1 and f[2]    == 1 then g[1]  = 1 else g[1]  = 0 end
    if f[#f] == 1 and f[#f-1] == 1 then g[#f] = 1 else g[#f] = 0 end
    f, g = g, f

    Output( f, l )
end
