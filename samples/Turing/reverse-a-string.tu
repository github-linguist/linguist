% Reverse a string

var input : string (100)

put "Enter a string to reverse: " ..
get input

var count : int := length(input)
loop
    if count >= 1 then
        put input(count) ..
    else
        exit
    end if
    count := count - 1
end loop
