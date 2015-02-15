require ("math")

shades = {'.', ':', '!', '*', 'o', 'e', '&', '#', '%', '@'}

function normalize (vec)
    len = math.sqrt(vec[1]^2 + vec[2]^2 + vec[3]^2)
    return {vec[1]/len, vec[2]/len, vec[3]/len}
end

light = normalize{30, 30, -50}

function dot (vec1, vec2)
    d = vec1[1]*vec2[1] + vec1[2]*vec2[2] + vec1[3]*vec2[3]
    return d < 0 and -d or 0
end

function draw_sphere (radius, k, ambient)
    for i = math.floor(-radius),-math.floor(-radius) do
        x = i + .5
        local line = ''
        for j = math.floor(-2*radius),-math.floor(-2*radius) do
            y = j / 2 + .5
            if x^2 + y^2 <= radius^2 then
                vec = normalize{x, y, math.sqrt(radius^2 - x^2 - y^2)}
                b = dot(light,vec) ^ k + ambient
                intensity = math.floor ((1 - b) * #shades)
                line = line .. (shades[intensity] or shades[1])
            else
                line = line .. ' '
            end
        end
        print (line)
    end
end

draw_sphere (20, 4, 0.1)
draw_sphere (10, 2, 0.4)
