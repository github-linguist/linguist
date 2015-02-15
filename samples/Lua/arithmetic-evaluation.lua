require"lpeg"

P, R, C, S, V = lpeg.P, lpeg.R, lpeg.C, lpeg.S, lpeg.V

--matches arithmetic expressions and returns a syntax tree
expression = P{"expr";
ws = P" "^0,
number = C(R"09"^1) * V"ws",
lp = "(" * V"ws",
rp = ")" * V"ws",
sym = C(S"+-*/") * V"ws",
more = (V"sym" * V"expr")^0,
expr = V"number" * V"more" + V"lp" * lpeg.Ct(V"expr" * V"more") * V"rp" * V"more"}

--evaluates a tree
function eval(expr)
  --empty
  if type(expr) == "string" or type(expr) == "number" then return expr + 0 end

  --arithmetic functions
  tb = {["+"] = function(a,b) return eval(a) + eval(b) end,
		["-"] = function(a,b) return eval(a) - eval(b) end,
		["*"] = function(a,b) return eval(a) * eval(b) end,
		["/"] = function(a,b) return eval(a) / eval(b) end}

  --you could add ^ or other operators to this pretty easily
  for i, v in ipairs{"*/", "+-"} do
    for s, u in ipairs(expr) do
	  local k = type(u) == "string" and C(S(v)):match(u)
	  if k then
	    expr[s-1] = tb[k](expr[s-1],expr[s+1])
	    table.remove(expr, s)
	    table.remove(expr, s)
	  end
	end
  end
  return expr[1]
end

print(eval{expression:match(io.read())})
