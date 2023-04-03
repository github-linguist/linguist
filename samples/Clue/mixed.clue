local bool_result = condition ? "true" : "false"
local result = 0
local output = "[Output] ";

result = match io.read("*n") {
	1 => 1,
	2 || 3 => 10
}

output ..= bool_result .. " " .. result

print(output)