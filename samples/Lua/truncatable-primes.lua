max_number = 1000000

numbers = {}
for i = 2, max_number do
    numbers[i] = i;
end

for i = 2, max_number do
    for j = i+1, max_number do
        if numbers[j] ~= 0 and j % i == 0 then numbers[j] = 0 end
    end
end

max_prime_left, max_prime_right = 2, 2
for i = 2, max_number do
    if numbers[i] ~= 0 then
        local is_prime = true

        local l = math.floor( i / 10 )
        while l > 1 do
            if numbers[l] == 0 then
                is_prime = false
                break
            end
            l = math.floor( l / 10 )
        end
        if is_prime then
            max_prime_left = i
        end

        is_prime = true
        local n = 10;
        while math.floor( i % 10 ) ~= 0 and n < max_number do
            if numbers[ math.floor( i % 10 ) ] ~= 0 then
                is_prime = false
                break
            end
            n = n * 10
        end
        if is_prime then
            max_prime_right = i
        end
    end
end

print( "max_prime_left = ", max_prime_left )
print( "max_prime_right = ", max_prime_right )
