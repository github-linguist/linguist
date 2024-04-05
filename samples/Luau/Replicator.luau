--// Authored by @ColRealPro (https://github.com/colrealpro)
--// Fetched from (https://github.com/ColRealPro/SimplyReplicate/blob/main/lib/Replicator.luau)
--// Licensed under the MIT License (https://github.com/ColRealPro/SimplyReplicate/blob/main/LICENSE)

--!strict

-- // Services
local Players = game:GetService("Players")
local RunService = game:GetService("RunService")

-- // Modules
local NetworkManager = require(script.Parent.NetworkManager)
local console = require(script.Parent.console)
local Maid = require(script.Parent.Maid)
local Signal = require(script.Parent.Signal)

-- console.WriteEnv()
local print = console.log

-- // Internal Types
type dataType<T> = T
type data = { [string]: dataType<any> }
type DataStructure = typeof(setmetatable({} :: data, {}))

type ReplicatorImpl = {
	__index: ReplicatorImpl,
	__tostring: (self: Replicator) -> string,
	
	new: (identifier: string | Instance, data: data) -> Replicator,
	changeStates: (self: Replicator, changedStates: data, players: ({ Player } | Player)?) -> (),
	syncPlayer: (self: Replicator, player: Player) -> (),
	_prepareReplication: (self: Replicator) -> (),
	_setState: (self: Replicator, state: string, value: any, players: { Player }?) -> (),
	_createDataStructure: (self: Replicator, data: data) -> DataStructure,
	_fetchFromServer: (self: Replicator) -> (),
	_recieveUpdate: (self: Replicator, data: { syncData: { Remove: { string }?, Modify: { { Index: string, NewValue: any } }? }?, updateData: { [string]: any } }) -> (),
	get: (self: Replicator, index: string?) -> { [string]: any } | any,
	getForPlayer: (self: Replicator, player: Player, index: string?) -> { [string]: any } | any,
	Destroy: (self: Replicator) -> ()
}

type Replicator = typeof(setmetatable({} :: {
	identifier: string | Instance,
	_maid: typeof(Maid),
	_networkManager: NetworkManager.NetworkManager,
	_isPlayer: boolean,
	_context: "Server" | "Client" | string,
	_deferringReplication: boolean,
	_replicationNeeded: { Updates: { { Index: string, NewValue: any, Players: { Player }?, updateOrderTime: number } }, Syncs: { { Remove: { string }, Modify: { { Index: string, NewValue: any } }, Player: Player, SyncOrderTime: number } } },
	-- _dataStructure: DataStructure,
	_localDataCopy: DataStructure,
	_userStateCopies: { [Player]: DataStructure },
	_originalDataStructure: { [string]: any },
	StateChanged: Signal.Signal<string, any>
}, {} :: ReplicatorImpl))

--[=[
	@class Replicator

	This is the main class of SimplyReplicate
	Replicators are used to replicate data from the server to the client with ease

	Here is an example implementation of a replicator:

	**Server:**
	```lua
	local Replicator = require(path.to.module)

	local GameStateReplicator = Replicator.new("GameState", {
		Status = "Waiting",
		RoundStarted = false,
	})

	task.wait(3)

	GameStateReplicator:changeStates({
		Status = "Game starts soon",
	})

	task.wait(3)

	GameStateReplicator:changeStates({
		Status = "Game started",
		RoundStarted = true,
	})	
	```

	**Client:**
	```lua
	local Replicator = require(path.to.module)

	local GameStateReplicator = Replicator.new("GameState")

	GameStateReplicator.StateChanged:Connect(function(state, value)
		print("State changed", state, value)
	end)
	```
]=]
local Replicator: ReplicatorImpl = {} :: ReplicatorImpl
Replicator.__index = Replicator
Replicator.__tostring = function(self)
	return `Replicator({tostring(self.identifier)})`
end

--[=[
	Constructs a new replicator

	@yields
	@function new
	@within Replicator
	@param identifier string | Instance
	@param data { [string]: any } -- The data that will be replicated to clients
	@return Replicator

	:::warning
	When creating a replicator on the client, this function will yield until the initial data has been fetched from the server
	:::

	:::info
	Note that if you are using strict luau, you will want to specify a type for the data parameter,
	if you let Luau infer the type, when you go to change the state, it will want you to include every state as it wont have an option to be nil.
	:::
]=]
function Replicator.new<T>(identifier: string | Instance, data: T?)
	local self = setmetatable({}, Replicator) :: Replicator

	self.identifier = identifier
	self._maid = Maid.new()
	self._networkManager = NetworkManager.new(identifier)
	self._isPlayer = typeof(identifier) == "Instance" and identifier:IsA("Player")
	self._context = RunService:IsServer() and "Server" or "Client"
	self._userStateCopies = {}
	
	-- // Signals
	self.StateChanged = Signal.new()
	
	if self._context == "Server" then
		if not data then
			error("Data must be provided when creating a replicator on the server.", 2)
		end

		if typeof(data) ~= "table" then
			error("Data must be a table.", 2)
		end

		self._replicationNeeded = {
			Updates = {},
			Syncs = {}
		}
		self._deferringReplication = false
		self._originalDataStructure = table.freeze(table.clone(data))
		self._localDataCopy = self:_createDataStructure(data)
		-- self._dataStructure = self:_createDataStructure(data)

		self._networkManager:listenToFetch("FETCH_REPLICATOR", function(player)
			if self._isPlayer and player ~= identifier then
				return
			end

			return self:get()
		end)
	else
		self:_fetchFromServer()
		self._networkManager:listen("UPDATE_REPLICATOR", function(data)
			if data.identifier == self.identifier then
				print("Recieved update from server")
				self:_recieveUpdate(data :: { syncData: { Remove: { string }?, Modify: { { Index: string, NewValue: any } }? }?, updateData: { [string]: any } })
			end
		end)
	end

	return self
end

--[=[
	Changes states in the replicator and replicate whatever changes to the clients

	@param changedStates { [string]: any? } -- The states that have been changed and their new values
	@error "Invalid state" -- Thrown when the state does not exist in the data structure
]=]
function Replicator:changeStates(changedStates: data, players: ({ Player } | Player)?)
	if self._context == "Client" then
		error("This function can only be called on the server", 2)
	end

	local playersTbl = type(players) == "table" and players or { players :: Player }

	for state, value in changedStates do
		self:_setState(state, value, players and playersTbl)
	end
end

--[=[
	Syncs a player's state with the server's state

	@param player Player
]=]
function Replicator:syncPlayer(player: Player)
	if self._context == "Client" then
		error("This function can only be called on the server", 2)
	end

	local userStateCopy = self._userStateCopies[player]

	if not userStateCopy then
		print(`{player.Name} is already in sync with the server`)
		return
	end

	local remove = {}
	local modify = {}

	for state, value in next, userStateCopy do
		if not rawget(self._localDataCopy :: any, state) then
			table.insert(remove, state)
		elseif self._localDataCopy[state] ~= value then
			-- FIXME: If a value is modified in the same frame before a sync, it will be put in the sync table even though it has not replication
			--        this isn't a huge issue but it leads to wasted data being sent to the client as the original value will be dropped during replication
			--        meaning that replicating the value as part of a sync is now useless
			table.insert(modify, {
				Index = state,
				NewValue = self._localDataCopy[state]
			})
		end
	end

	table.insert(self._replicationNeeded.Syncs, {
		Remove = remove,
		Modify = modify,
		Player = player,

		SyncOrderTime = os.clock()
	})

	self:_prepareReplication()
end

function Replicator:_prepareReplication()
	print("Preparing replication")
	self._deferringReplication = true

	task.defer(function()
		self._deferringReplication = false

		local replicationNeeded = self._replicationNeeded
		self._replicationNeeded = {
			Updates = {},
			Syncs = {}
		}

		local replicationData = {
			["$all"] = {
				updateData = {},
			}
		}

		type PlayerReplicationData = {
			syncData: { Remove: { string }?, Modify: { { Index: string, NewValue: any } }? },
			updateData: { [string]: any },
			internalData: { syncOrderTime: number? }
		}

		local playerReplicationDataTemplate = {
			syncData = {},
			updateData = {},
			internalData = {}
		}

		-- Evaluate syncs first to make sure that we have enough data to keep the replication in order
		-- and have the client in sync with the server
		for _, v in replicationNeeded.Syncs do
			local player = v.Player
			local remove = v.Remove
			local modify = v.Modify
			local syncOrderTime = v.SyncOrderTime
			
			if not replicationData[player] then
				replicationData[player] = table.clone(playerReplicationDataTemplate) :: PlayerReplicationData
			end

			local previousSyncOrderTime = replicationData[player].internalData.syncOrderTime
			if previousSyncOrderTime and syncOrderTime < previousSyncOrderTime then
				print(`Dropping sync for player {player.Name} as a newer sync has already been evaluated`)
				continue
			end

			replicationData[player].syncData.Remove = remove
			replicationData[player].syncData.Modify = modify

			replicationData[player].internalData.syncOrderTime = syncOrderTime
		end

		for _, v in replicationNeeded.Updates do
			local players = v.Players
			local index = v.Index
			local newValue = v.NewValue

			if players then
				for _, player in players do
					if not replicationData[player] then
						replicationData[player] = table.clone(playerReplicationDataTemplate) :: PlayerReplicationData
					end

					local syncOrderTime = replicationData[player].internalData.syncOrderTime
					if syncOrderTime and v.updateOrderTime < syncOrderTime then
						print(`Dropping update for state {v.Index} for player {player.Name} as a sync has already been evaluated after this update`)
						continue
					end
	
					replicationData[player].updateData[index] = newValue
				end
			else
				replicationData["$all"].updateData[index] = newValue
			end
		end

		print("About to replicate data, sending out:", replicationData, "created from:", replicationNeeded)

		self._networkManager:send("UPDATE_REPLICATOR", {
			identifier = self.identifier,
			updateData = replicationData["$all"].updateData,
		})

		for player: Player | string, data in replicationData do
			-- if player == "$all" then
			-- 	self._networkManager:send("UPDATE_REPLICATOR", {
			-- 		identifier = self.identifier,
			-- 		updateData = data.updateData,
			-- 	})
			-- 	continue
			-- end

			if player == "$all" then
				continue
			end

			self._networkManager:send("UPDATE_REPLICATOR", {
				identifier = self.identifier,
				syncData = data.syncData,
				updateData = data.updateData,
			}, {player :: Player})
		end
	end)
end

function Replicator:_setState(state: string, value: any, players: { Player }?)
	-- self._dataStructure[state] = value
	-- self._replicationNeeded[state] = value

	table.insert(self._replicationNeeded.Updates, {
		Index = state,
		NewValue = value,
		Players = players,
		updateOrderTime = os.clock()
	})

	if not players then
		self._localDataCopy[state] = value
		players = Players:GetPlayers()

		task.defer(function()
			self.StateChanged:Fire(state, value)
		end)
	end

	for _, player in players :: { Player } do
		if not self._userStateCopies[player] then
			self._userStateCopies[player] = self:_createDataStructure(self._originalDataStructure)
		end

		self._userStateCopies[player][state] = value
	end
	
	if not self._deferringReplication then
		self:_prepareReplication()
	end

	print(`Successfully changed state {state} to {value}`)
end

function Replicator:_createDataStructure(data: { [string]: any }) :  DataStructure
	local dataStructure = setmetatable(table.clone(data), {
		__index = function(self, key)
			error(string.format("Invalid state %s, does not exist in the data structure.",
				tostring(key)), 4)
		end,
		__newindex = function(self, key, value)
			error(string.format("Invalid state %s, does not exist in the data structure.",
				tostring(key)), 4)
		end
	})

	return dataStructure
end

function Replicator:_fetchFromServer()
	local data = self._networkManager:fetch("FETCH_REPLICATOR", {
		identifier = self.identifier,
	}) :: { [string]: any }

	self._localDataCopy = self:_createDataStructure(data)

	print("Successfully fetched initial data from server")
end

function Replicator:_recieveUpdate(data: { syncData: { Remove: { string }?, Modify: { { Index: string, NewValue: any } }? }?, updateData: { [string]: any } })
	-- for state, value in data do
	-- 	self._localDataCopy[state] = value
		
	-- 	task.defer(function()
	-- 		self.StateChanged:Fire(state, value)
	-- 	end)
	-- end

	-- print("Handled update from server")

	local syncData = data.syncData
	local updateData = data.updateData

	if syncData then
		local remove = syncData.Remove
		local modify = syncData.Modify

		if remove then
			for _, state in remove do
				self._localDataCopy[state] = nil

				task.defer(function()
					if self._localDataCopy[state] then
						return
					end

					self.StateChanged:Fire(state, nil)
				end)
			end
		end

		if modify then
			for _, v in modify do
				local index = v.Index
				local newValue = v.NewValue

				if self._localDataCopy[index] == newValue then
					print(`Droping update for state {index} as the value is already the same`)
					continue
				end

				self._localDataCopy[index] = newValue
				
				task.defer(function()
					if self._localDataCopy[index] ~= newValue then
						return
					end

					self.StateChanged:Fire(index, newValue)
				end)
			end
		end
	end

	for state, value in updateData do
		self._localDataCopy[state] = value

		task.defer(function()
			if self._localDataCopy[state] ~= value then
				return
			end

			self.StateChanged:Fire(state, value)
		end)
	end
end

--[=[
	Returns the current state stored in the replicator

	@error "Invalid state" -- Thrown when the state does not exist in the data structure
	@param index string? -- If provided, returns the state given, otherwise returns all states
]=]
function Replicator:get(index: string?): { [string]: any } | any
	if index then
		return self._localDataCopy[index]
	end

	local dataStructure = self._localDataCopy
	local data = {}

	-- Manually clone the table to remove the metatable
	for state, value in next, dataStructure do
		data[state] = value
	end

	return data
end

--[=[
	Returns the current state stored in the replicator for a specific player

	@error "Invalid state" -- Thrown when the state does not exist in the data structure
	@param player Player
	@param index string? -- If provided, returns the state given, otherwise returns all states
]=]
function Replicator:getForPlayer(player: Player, index: string?): { [string]: any } | any
	if self._context == "Client" then
		error("This function can only be called on the server", 2)
	end

	if not self._userStateCopies[player] then
		return self:get(index)
	end

	if index then
		return self._userStateCopies[player][index]
	end

	local dataStructure = self._userStateCopies[player]
	local data = {}

	-- Manually clone the table to remove the metatable
	for state, value in next, dataStructure do
		data[state] = value
	end

	return data
end

--[=[
	This function should be called when you are done using the replicator
]=]
function Replicator:Destroy()
	self._networkManager:Destroy()
	self._maid:DoCleaning()

	table.clear(self :: any)
	setmetatable(self :: any, {
		__metatable = "The metatable is locked",
		__index = function()
			error("This replicator has been destroyed", 2)
		end,
		__newindex = function()
			error("This replicator has been destroyed", 2)
		end
	})
end

return Replicator
