local envs = { }
for i = 1, 12 do
    -- fallback to the global environment for io and math
    envs[i] = setmetatable({ count = 0, n = i }, { __index = _G })
end

local code = [[
io.write(("% 4d"):format(n))
if n ~= 1 then
    count = count + 1
    n = (n % 2 == 1) and 3 * n + 1 or math.floor(n / 2)
end
]]

while true do
    local finished = 0
    for _, env in ipairs(envs) do
        if env.n == 1 then finished = finished + 1 end
    end
	
    if finished == #envs then break end

    for _, env in ipairs(envs) do
        -- 5.1; in 5.2, use load(code, nil, nil, env)() instead
        setfenv(loadstring(code), env)()
    end
    io.write "\n"
end

print "counts:"
for _, env in ipairs(envs) do
    io.write(("% 4d"):format(env.count))
end
