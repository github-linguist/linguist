local target = "METHINKS IT IS LIKE A WEASEL"
local alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
local c, p = 100, 0.06

local function fitness(s)
	local score = #target
	for i = 1,#target do
		if s:sub(i,i) == target:sub(i,i) then score = score - 1 end
	end
	return score
end

local function mutate(s, rate)
	local result, idx = ""
	for i = 1,#s do
		if math.random() < rate then
			idx = math.random(#alphabet)
			result = result .. alphabet:sub(idx,idx)
		else
			result = result .. s:sub(i,i)
		end
	end
	return result, fitness(result)
end

local function randomString(len)
	local result, idx = ""
	for i = 1,len do
		idx = math.random(#alphabet)
		result = result .. alphabet:sub(idx,idx)
	end
	return result
end

local function printStep(step, s, fit)
	print(string.format("%04d: ", step) .. s .. " [" .. fit .."]")
end

math.randomseed(os.time())
local parent = randomString(#target)
printStep(0, parent, fitness(parent))

local step = 0
while parent ~= target do
	local bestFitness, bestChild, child, fitness = #target + 1
	for i = 1,c do
		child, fitness = mutate(parent, p)
		if fitness < bestFitness then bestFitness, bestChild = fitness, child end
	end
	parent, step = bestChild, step + 1
	printStep(step, parent, bestFitness)
end
