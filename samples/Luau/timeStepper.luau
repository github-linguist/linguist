--// Authored by @AmaranthineCodices (https://github.com/AmaranthineCodices)
--// Fetched from (https://github.com/Floral-Abyss/recs/blob/be123afef8f1fddbd9464b7d34d5178393601ae3/recs/TimeStepper.luau)
--// Licensed under the MIT License (https://github.com/Floral-Abyss/recs/blob/master/LICENSE.md)

--!strict

--[[

A time stepper is responsible for stepping systems in a deterministic order at a set interval.

]]

local TimeStepper = {}
TimeStepper.__index = TimeStepper

function TimeStepper.new(interval: number, systems)
	local self = setmetatable({
		_systems = systems,
		_interval = interval,
	}, TimeStepper)

	return self
end

function TimeStepper:start()
	coroutine.resume(coroutine.create(function()
		while true do
			local timeStep, _ = wait(self._interval)
			for _, system in ipairs(self._systems) do
				system:step(timeStep)
			end
		end
	end))
end

return TimeStepper
