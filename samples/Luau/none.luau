--// Authored by @benbrimeyer (https://github.com/benbrimeyer)
--// Fetched from (https://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/source/None.luau)
--// Licensed under the MIT License (https://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/LICENSE)

--[=[
	@prop None None
	@within Freeze

	Since lua tables cannot distinguish between values not being present and a value of nil,
	`Freeze.None` exists to represent values that should be interpreted as `nil`.

	This is useful when removing values with functions such as [`Freeze.Dictionary.merge`](../api/Dictionary#merge).
]=]

local None = newproxy(true)

getmetatable(None).__tostring = function()
	return "Freeze.None"
end

return None
