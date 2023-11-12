--// Authored by @sinlerdev (https://github.com/sinlerdev)
--// Fetched from (https://github.com/vinum-team/Vinum/blob/b80a6c194e6901f9d400968293607218598e1386/src/Utils/createProcessor.luau)
--// Licensed under the MIT License (https://github.com/vinum-team/Vinum/blob/b80a6c194e6901f9d400968293607218598e1386/LICENSE)

--!strict
local function createProcessor(valueChecker: (a: any, b: any) -> boolean, cleaner: (taskItem: any) -> ())
	return function<T>(old: T, new: T, isDestroying: boolean)
		if isDestroying then
			cleaner(old)
			return false
		end

		if valueChecker(old, new) then
			return false
		end

		cleaner(old)
		return true
	end
end

return createProcessor
