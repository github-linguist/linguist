
local ans = {
{1,1,1,1};
{2,2,2,2};
{1,2,1,2};
{3,3,3,3};
{1,1,3,3};
{0,2,3,0};
{1,2,3,0};
{4,4,4,4};
{1,0,0,4};
{2,2,4,4};
{1,2,0,4};
{3,4,3,4};
{1,0,3,4};
{0,2,3,4};
{1,2,3,4};
}
function create(foo,a,b,c,d)
	if a then
		terra foo(a : int, b : int)
			return 1
		end
	end
	if b then
		terra foo(a : int, b : double)
			return 2
		end
	end
	if c then
		terra foo(a : double, b : int)
			return 3
		end
	end
	if d then
		terra foo(a : double, b : double)
			return 4
		end
	end
end

local function bitset(i,b)
	return bit.band(bit.rshift(i,b),1) == 1
end

for i = 1,15 do
	local terra foo
	local a,b,c,d = bitset(i,0),bitset(i,1),bitset(i,2),bitset(i,3)
	create(foo, a,b,c,d)
	local function trycall(arg1,arg2)
		local r = 0
		pcall(function()
			local terra testit()
				var a : arg1, b : arg2 = 0,0
				return foo(a,b)
			end
			r = testit()
		end)
		return r
	end
	local r = {trycall(int,int),trycall(int,double),trycall(double,int),trycall(double,double)}
	for j,rr in ipairs(r) do
		assert(rr == ans[i][j])
	end
end