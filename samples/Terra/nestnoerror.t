

terra haserror()
	return (1):foo()
end

local m = macro(function()
	local success = pcall(function() haserror:compile() end)
	assert(not success)
	return 1
end)

terra noerror()
	return m()
end

assert(1 == noerror())