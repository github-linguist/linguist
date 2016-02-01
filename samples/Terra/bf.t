
local C = terralib.includec("stdio.h")

local function compile(code,N)
	local function body(data,ptr)
		local stmts = terralib.newlist()
		local jumpstack = {}
		for i = 1,#code do
			local c = code:sub(i,i)
			local stmt
			if c == ">" then
				stmt = quote ptr = ptr + 1 end
			elseif c == "<" then
				stmt = quote ptr = ptr - 1 end
			elseif c == "+" then
				stmt = quote data[ptr] = data[ptr] + 1 end
			elseif c == "-" then
				stmt = quote data[ptr] = data[ptr] - 1 end
			elseif c == "." then
				stmt = quote C.putchar(data[ptr]) end
			elseif c == "," then
				stmt = quote data[ptr] = C.getchar() end
			elseif c == "[" then
				local target = { before = symbol(), after = symbol() }
				table.insert(jumpstack,target)
				stmt = quote 
					::[target.before]:: 
					if data[ptr] == 0 then
						goto [target.after]
					end
				end
			elseif c == "]" then
				local target = table.remove(jumpstack)
				assert(target)
				stmt = quote 
					goto [target.before]
					:: [target.after] ::
				end
			else
				error("unknown character "..c)
			end
			stmts:insert(stmt)
		end
		return stmts
	end
	return terra()
		var data : int[N]
		for i = 0, N do
			data[i] = 0
		end
		var ptr = 0;
		[ body(data,ptr) ]
	end
end

local helloworld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

local fn = compile(helloworld,256)

fn()