number = {}

function number.pow( a, b )
    local ret = 1
    if b >= 0 then
        for i = 1, b do
            ret = ret * a.val
        end
    else
        for i = b, -1 do
            ret = ret / a.val
        end
    end
    return ret
end

function number.New( v )
    local num = { val = v }
    local mt = { __pow = number.pow }
    setmetatable( num, mt )
    return num
end

x = number.New( 5 )
print( x^2 )                   --> 25
print( number.pow( x, -4 ) )   --> 0.016
