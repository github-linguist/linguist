local terra foo() : {}
	bar()
end and 
local terra bar() : {}
	foo()
end

bar = nil

foo:printpretty()