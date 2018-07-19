function halve(a)
    return a/2
end

function double(a)
    return a*2
end

function isEven(a)
    return a%2 == 0
end

function ethiopian(x, y)
    local result = 0

    while (x >= 1) do
        if not isEven(x) then
            result = result + y
        end

        x = math.floor(halve(x))
        y = double(y)
    end

    return result;
end

print(ethiopian(17, 34))
