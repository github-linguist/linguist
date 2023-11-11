--[[
Simple signal/slot implementation
]]
local signal_mt = {
    __index = {
        register = table.insert
    }
}
function signal_mt.__index:emit(... --[[ Comment in params ]])
    for _: number, slot: any in ipairs(self) do
        slot(self, ...)
    end
end
local function create_signal(): typeof(signal_mt)
    return setmetatable({}, signal_mt)
end

-- Signal test
local signal: typeof(signal_mt) = create_signal()
signal:register(function(_signal, ...)
    print(...)
end)
signal:emit('Answer to Life, the Universe, and Everything:', 42)

-- Output test
local testNumber: number = 10

print(`this is line, and "testNumber" equals to: {testNumber}.`)

--[==[ [=[ [[
Nested ]]
multi-line ]=]
comment ]==]
[==[ Nested
[=[ multi-line
[[ string
]] ]=] ]==]