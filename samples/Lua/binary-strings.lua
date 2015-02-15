foo = 'foo'             -- Ducktyping foo to be string 'foo'
bar = 'bar'
assert (foo == "foo")   -- Comparing string var to string literal
assert (foo ~= bar)
str = foo               -- Copy foo contents to str
if #str == 0 then       -- # operator returns string length
    print 'str is empty'
end
str=str..string.char(50)-- Char concatenated with .. operator
substr = str:sub(1,3)   -- Extract substring from index 1 to 3, inclusively

str = "string string string string"
-- This function will replace all occurances of 'replaced' in a string with 'replacement'
function replaceAll(str,replaced,replacement)
    local function sub (a,b)
        if b > a then
            return str:sub(a,b)
        end
        return nil
    end
    a,b = str:find(replaced)
    while a do
        str = str:sub(1,a-1) .. replacement .. str:sub(b+1,#str)
        a,b = str:find(replaced)
    end
    return str
end
str = replaceAll (str, 'ing', 'ong')
print (str)

str = foo .. bar -- Strings concatenate with .. operator
