#!/usr/bin/env lua

function main(arg)
	local program = arg[0]
	print("Program: " .. program)
end

if type(package.loaded[(...)]) ~= "userdata" then
	main(arg)
else
	module(..., package.seeall)
end
