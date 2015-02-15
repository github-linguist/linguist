-- utility method to escape punctuation
function normalize(str)
	local result = str:gsub("(%p)", "%%%1")
	-- print(result)
	return result
end

-- utility method to split string into lines
function get_lines(str)
	local t = {}
	for line in str:gmatch("([^\n\r]*)[\n\r]*") do
		table.insert(t, line)
	end
	return t
end

local markov = {}
local MARKOV_RULE_PATTERN = "(.+)%s%-%>%s(%.?)(.*)"

function markov.rule(pattern,replacement,terminating)
	return {
		pattern = pattern,
		replacement = replacement,
		terminating = (terminating == ".")
	}, normalize(pattern)
end

function markov.make_rules(sample)
	local lines = get_lines(sample)
	local rules = {}
	local finders = {}
	for i,line in ipairs(lines) do
		if not line:find("^#") then
		s,e,pat,term,rep = line:find(MARKOV_RULE_PATTERN)
		if s then
			r, p = markov.rule(pat,rep,term)
			rules[p] = r
			table.insert(finders, p)
		end
		end
	end
	return {
		rules = rules,
		finders = finders
	}
end

function markov.execute(state, sample_input)

local rules, finders = state.rules, state.finders
local found = false -- did we find any rule?
local terminate = false

repeat
found = false

for i,v in ipairs(finders) do
	local found_now = false -- did we find this rule?
	if sample_input:find(v) then
		found = true
		found_now = true
	end
	sample_input = sample_input:gsub(v, rules[v].replacement, 1)
	-- handle terminating rules
	if found_now then
		if rules[v].terminating then terminate = true end
		break
	end
end

until not found or terminate

return sample_input
end
------------------------------------------
------------------------------------------

local grammar1 = [[
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar2 = [[
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar3 = [[
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar4 = [[
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
]]

local grammar5 = [[
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
]]

local text1 = "I bought a B of As from T S."
local text2 = "I bought a B of As W my Bgage from T S."
local text3 = '_1111*11111_'
local text4 = '000000A000000'

------------------------------------------
------------------------------------------

function do_markov(rules, input, output)
	local m = markov.make_rules(rules)
	input = markov.execute(m, input)
	assert(input == output)
	print(input)
end

do_markov(grammar1, text1, 'I bought a bag of apples from my brother.')
do_markov(grammar2, text1, 'I bought a bag of apples from T shop.')
-- stretch goals
do_markov(grammar3, text2, 'I bought a bag of apples with my money from T shop.')
do_markov(grammar4, text3, '11111111111111111111')
do_markov(grammar5, text4, '00011H1111000')
