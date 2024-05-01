--// Authored by @AmaranthineCodices (https://github.com/AmaranthineCodices)
--// Fetched from (https://github.com/Floral-Abyss/recs/blob/be123afef8f1fddbd9464b7d34d5178393601ae3/recs/createSignal.luau)
--// Licensed under the MIT License (https://github.com/Floral-Abyss/recs/blob/master/LICENSE.md)

--!strict

local function createSignal()
	local listeners = {}
	local signal = {}

	function signal:Connect(listener)
		listeners[listener] = true

		local connection = {
			Connected = true,
		}

		function connection.Disconnect()
			connection.Connected = false
			listeners[listener] = nil
		end
		connection.disconnect = connection.Disconnect

		return connection
	end
	signal.connect = signal.Connect

	local function fire(...)
		for listener, _ in pairs(listeners) do
			spawn(listener, ...)
		end
	end

	return signal, fire
end

return createSignal
