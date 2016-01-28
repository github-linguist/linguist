c = global(int,0)

terra up()
    c = c + 1
end

terra foo()
    var a = 0
    while([quote do defer up() end in a < 10 end]) do
        a = a + 1
    end
end

foo()
assert(c:get() == 11)

terra foo2()
    var a = 0
    while(a < 10 and [quote do defer up() end in true end]) do
        a = a + 1
    end
end

foo2()
assert(c:get() == 21)

terra foo3()
    var a = 0
    while true do
        var r = a < 10 and [quote do defer up() end in true end]
        if not r then break end
        a = a + 1
    end
end
foo3()
assert(c:get() == 31)

function failit(match,fn)
	local success,msg = pcall(fn)
	if success then
		error("failed to fail.",2)
	elseif not string.match(msg,match) then
		error("failed wrong: "..msg,2)
	end
end
local df = "defer statements are not allowed in conditional expressions"
failit(df,function()
    local terra foo()
        if [quote defer up() in true end] then end
    end
    foo()
end)
failit(df,function()
    local terra foo()
        while [quote defer up() in true end] do end
    end
    foo()
end)
failit(df,function()
    local terra foo()
        repeat until [quote defer up() in true end] do end
    end
    foo()
end)
failit(df,function()
    local terra foo()
        var a = true or [quote defer up() in true end]
    end
    foo()
end)

failit(df,function()
    local terra foo()
        var a = [quote defer up() in true end] and true
    end
    foo()
end)
    local terra foo()
        var a = [quote defer up() in 1 end] and 2
    end
    foo()
    assert(c:get() == 32)