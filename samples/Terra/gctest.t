



local terra foo()
end

local terra bar()
end

foo:compile()
bar:compile()
foo = nil

collectgarbage()
collectgarbage()

print("HERE")