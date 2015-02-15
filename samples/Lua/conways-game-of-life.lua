function Evolve( cell )
    local m = #cell
    local cell2 = {}
    for i = 1, m do
        cell2[i] = {}
        for j = 1, m do
            cell2[i][j] = cell[i][j]
        end
    end

    for i = 1, m do
        for j = 1, m do
            local count
            if cell2[i][j] == 0 then count = 0 else count = -1 end
            for x = -1, 1 do
                for y = -1, 1 do
                    if i+x >= 1 and i+x <= m and j+y >= 1 and j+y <= m and cell2[i+x][j+y] == 1 then count = count + 1 end
                end
            end
            if count < 2 or count > 3 then cell[i][j] = 0 end
            if count == 3 then cell[i][j] = 1 end
        end
    end

    return cell
end


m = 3                   -- number rows / colums
num_iterations = 10

cell = {}
for i = 1, m do
    cell[i] = {}
    for j = 1, m do
        cell[i][j] = 0
    end
end

cell[2][2], cell[2][1], cell[2][3] = 1, 1, 1

for l = 1, num_iterations do
    for i = 1, m do
        for j = 1, m do
            if cell[i][j] == 1 then io.write( "#" ) else io.write( " " ) end
        end
        io.write( "\n" )
    end

    cell = Evolve( cell )
end
