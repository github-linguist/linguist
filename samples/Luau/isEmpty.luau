--// Authored by @benbrimeyer (https://github.com/benbrimeyer)
--// Fetched from (https://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/source/isEmpty.luau)
--// Licensed under the MIT License (https://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/LICENSE)

--!strict
--[=[
	Returns true if the collection is empty.

	```lua
	Freeze.isEmpty({})
	-- true
	```

	@within Freeze
	@function isEmpty
	@return boolean
]=]

return function(collection)
	return next(collection) == nil
end
