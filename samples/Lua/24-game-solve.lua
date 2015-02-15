local SIZE = #arg[1]
local GOAL = tonumber(arg[2]) or 24

local input = {}
for v in arg[1]:gmatch("%d") do
	table.insert(input, v)
end
assert(#input == SIZE, 'Invalid input')

local operations = {'+', '-', '*', '/'}

local function BinaryTrees(vert)
	if vert == 0 then
		return {false}
	else
		local buf = {}
		for leften = 0, vert - 1 do
			local righten = vert - leften - 1
			for _, left in pairs(BinaryTrees(leften)) do
				for _, right in pairs(BinaryTrees(righten)) do
					table.insert(buf, {left, right})
				end
			end
		end
		return buf
	end
end
local trees = BinaryTrees(SIZE-1)
local c, opc, oper, str
local max = math.pow(#operations, SIZE-1)
local function op(a,b)
	opc = opc + 1
	local i = math.floor(oper/math.pow(#operations, opc-1))%#operations+1
	return '('.. a .. operations[i] .. b ..')'
end

local function EvalTree(tree)
	if tree == false then
		c = c + 1
		return input[c-1]
	else
		return op(EvalTree(tree[1]), EvalTree(tree[2]))
	end
end

local function printResult()
	for _, v in ipairs(trees) do
		for i = 0, max do
			c, opc, oper = 1, 0, i
			str = EvalTree(v)
			loadstring('res='..str)()
			if(res == GOAL) then print(str, '=', res) end
		end
	end
end

local uniq = {}
local function permgen (a, n)
	if n == 0 then
		local str = table.concat(a)
		if not uniq[str] then
			printResult()
			uniq[str] = true
		end
	else
		for i = 1, n do
			a[n], a[i] = a[i], a[n]
			permgen(a, n - 1)
			a[n], a[i] = a[i], a[n]
		end
	end
end

permgen(input, SIZE)
