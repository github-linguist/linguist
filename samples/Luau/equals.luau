--// Authored by @sinlerdev (https://github.com/sinlerdev)
--// Fetched from (https://github.com/vinum-team/Vinum/blob/b80a6c194e6901f9d400968293607218598e1386/src/Utils/equals.luau)
--// Licensed under the MIT License (https://github.com/vinum-team/Vinum/blob/b80a6c194e6901f9d400968293607218598e1386/LICENSE)

--!strict

local function equals(a: any, b: any): boolean
	-- INFO: Vinum doesn't officially support immutability currently- so, we just assume
	-- that every new table entry is not equal. See issue 26
	if type(a) == "table" then
		return false
	end

	return a == b
end

return equals
