--// Authored by @benbrimeyer (https://github.com/benbrimeyer)
--// Fetched from (https://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/source/List/toSet.luau)
--// Licensed under the MIT License (hhttps://github.com/duckarmor/Freeze/blob/670bb6593d8cc54cdcbb96cd94d1273ee0420abb/LICENSE)

--!strict

--[=[
	Returns a Set from the given List.

	@within List
	@function toSet
	@ignore
]=]
return function<Value>(list: { Value }): { [Value]: boolean }
	local set = {}

	for _, v in list do
		set[v] = true
	end

	return set
end
