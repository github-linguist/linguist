function SetSeed( f )
    for i = 1, #f[1] do         -- the whole boundary of the scene is used as the seed
        f[1][i]  = 1
        f[#f][i] = 1
    end
    for i = 1, #f do
        f[i][1]     = 1
        f[i][#f[1]] = 1
    end
end

function SetParticle( f )
    local pos_x, pos_y
    repeat
        pos_x = math.random( #f )
        pos_y = math.random( #f[1] )
    until f[pos_x][pos_y] == 0

    return pos_x, pos_y
end


function Iterate( f, num_particles )
    for i = 1, num_particles do
        local pos_x, pos_y = SetParticle( f )

        while true do
            local dx = math.random(5) - 3
            local dy = math.random(5) - 3

            if ( pos_x+dx >= 1 and pos_x+dx <= #f and pos_y+dy >= 1 and pos_y+dy <= #f[1] ) then
                if f[pos_x+dx][pos_y+dy] ~= 0 then
                    f[pos_x][pos_y] = 1
                    break
                else
                    pos_x = pos_x + dx
                    pos_y = pos_y + dy
                end
            end
        end
    end
end


size_x, size_y = 400, 400       -- size of the scene
num_particles  = 16000

math.randomseed( os.time() )

f = {}
for i = 1, size_x do
    f[i] = {}
    for j = 1, size_y do
        f[i][j] = 0
    end
end

SetSeed( f )
Iterate( f, num_particles )

-- prepare the data for writing into a ppm-image file
for i = 1, size_x do
    for j = 1, size_y do
        if f[i][j] == 1 then f[i][j] = 255 end
    end
end
Write_PPM( "brownian_tree.ppm", ConvertToColorImage(f) )
