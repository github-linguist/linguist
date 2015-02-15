function dragon()
    local l = "l"
    local r = "r"
    local inverse = {l = r, r = l}
    local field = {r}
    local num = 1
    local loop_limit = 6 --increase this number to render a bigger curve
    for discard=1,loop_limit do
        field[num+1] = r
        for i=1,num do
            field[i+num+1] = inverse[field[num-i+1]]
        end
        num = num*2+1
    end
    return field
end

function render(field, w, h, l)
    local x = 0
    local y = 0
    local points = {}
    local highest_x = 0
    local highest_y = 0
    local lowest_x = 0
    local lowest_y = 0
    local l = "l"
    local r = "r"
    local u = "u"
    local d = "d"
    local heading = u
    local turn = {r = {r = d, d = l, l = u, u = r}, l = {r = u, u = l, l = d, d = r}}
    for k, v in ipairs(field) do
        heading = turn[v][heading]
        for i=1,3 do
            points[#points+1] = {x, y}
            if heading == l then
                x = x-w
            elseif heading == r then
                x = x+w
            elseif heading == u then
                y = y-h
            elseif heading == d then
                y = y+h
            end
            if x > highest_x then
                highest_x = x
            elseif x < lowest_x then
                lowest_x = x
            end
            if y > highest_y then
                highest_y = y
            elseif y < lowest_y then
                lowest_y = y
            end
        end
    end
    points[#points+1] = {x, y}
    highest_x = highest_x - lowest_x + 1
    highest_y = highest_y - lowest_y + 1
    for k, v in ipairs(points) do
        v[1] = v[1] - lowest_x + 1
        v[2] = v[2] - lowest_y + 1
    end
    return highest_x, highest_y, points
end

function render_text_mode()
    local width, height, points = render(dragon(), 1, 1, 1)
    local rows = {}
    for i=1,height do
        rows[i] = {}
        for j=1,width do
            rows[i][j] = ' '
        end
    end
    for k, v in ipairs(points) do
        rows[v[2]][v[1]] = "*"
    end

    for i=1,height do
        print(table.concat(rows[i], ""))
    end
end

function dump_points()
    local width, height, points = render(dragon(), 4, 4, 1)
    for k, v in ipairs(points) do
        print(unpack(v))
    end
end

--replace this line with dump_points() to output a list of coordinates:
render_text_mode()
