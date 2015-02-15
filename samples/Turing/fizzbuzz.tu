setscreen("nocursor,noecho")

for i : 1 .. 100
    if i mod 15 = 0 then
        put "Fizzbuzz" ..
    elsif i mod 5 = 0 then
        put "Buzz" ..
    elsif i mod 3 = 0 then
        put "Fizz" ..
    else
        put i ..
    end if
end for
