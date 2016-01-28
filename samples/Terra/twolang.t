
do
import "lib/foolang"
a = foo bar
import "lib/barlang"
b = bar foo
assert( foo bar + bar foo == 3)
assert(a + b == 3)
end

local function failparse(str,match)
	local r,msg = terralib.loadstring(str)
	assert(not r)
	local match = msg:match(match) 
	if not match then print(msg) end
	assert(match)
end

failparse("return (foo bar + bar foo)", "near 'bar'")
failparse('import "lib/foolang";return (foo bar + bar foo)', "near foo")
failparse('import "lib/foolang"; import "lib/foolang";',"entrypoint 'foo' already defined")

do import "lib/foolang" end do import "lib/foolang" end