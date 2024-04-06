--// Authored by @Dig1t (https://github.com/dig1t)
--// Fetched from (https://github.com/dig1t/dlib/blob/main/src/Maid.luau)
--// Licensed under the MIT License (https://github.com/dig1t/dlib/blob/main/LICENSE)

--!optimize 2
--!strict
--!native

local Util = require(script.Parent.Util)

type MaidClass = {
	__index: MaidClass,
	new: () -> MaidType,
	task: (self: MaidType, task: any, cleaner: () -> any) -> string,
	removeTask: (self: MaidType, taskToRemove: MaidTask) -> nil,
	clean: (self: MaidType) -> nil,
	destroy: (any) -> nil,

	MaidType: MaidType
}

type MaidInstance = {
	_tasks: { [string]: MaidTask },
	[any]: any
}

export type MaidType = typeof(setmetatable(
	{} :: MaidInstance,
	{} :: MaidClass
))

type MaidTask = {
	Connected: boolean?,
	Disconnect: () -> nil?,
	Destroy: (any) -> nil?,
	destroy: (any) -> nil?,
	destructor: (task: any) -> nil?,
	[any]: any
}? | () -> nil? | Instance;

--[=[
	Task management class for cleaning up things for garbage collection.

	@class Maid
]=]
local Maid: MaidClass = {} :: MaidClass
Maid.__index = Maid

--[=[
	Creates a new Maid instance.

	```lua
	local maid = Maid.new()
	```

	@return Maid
]=]
function Maid.new(): MaidType
	local self = setmetatable({}, Maid)

	self._tasks = {}

	return self
end

--[=[
	Adds a task to the Maid instance.

	```lua
	local maid = Maid.new()

	maid:task(function()
		print("Hello world!")
	end)
	```

	Multiple types of tasks can be added to the Maid instance.

	```lua
	local maid = Maid.new()

	-- Functions
	maid:task(function()
		print("Hello world!")
	end)

	-- RBXScriptConnections
	maid:task(workspace.ChildAdded:Connect(function()
		print("Hello world!")
	end))

	-- Instances with "Destroy" methods
	maid:task(Instance.new("Part"))

	-- Packages with "Destroy", "destroy", or "destructor" methods
	local instance = Class.new({
		PackageVariable = "Hello world!",
		Destroy = function()
			-- destroy this package instance
		end
	})

	maid:task(instance)
	```

	@param _task any -- The task to add.
	@return string -- The task id.
]=]
function Maid:task(_task: MaidTask): string
	local taskId = Util.randomString(14)

	self._tasks[taskId] = _task

	return taskId
end

--[=[
	Removes a task from the Maid instance.

	```lua
	local maid = Maid.new()

	local taskId = maid:task(function()
		print("Hello world!")
	end)

	maid:removeTask(taskId)
	```

	@param taskToRemove any -- The task item to remove.
	@return nil
]=]
function Maid:removeTask(taskToRemove: string | MaidTask): nil
	-- Remove by task id
	if typeof(taskToRemove) == "string" then
		self._tasks[taskToRemove] = nil

		return
	end

	-- Remove by task
	for taskId, _task: MaidTask in pairs(self._tasks) do
		if _task == taskToRemove then
			self._tasks[taskId] = nil
		end
	end

	return
end

--[=[
	Cleans up all tasks in the Maid instance.

	```lua
	local maid: typeof(Maid.MaidType) = Maid.new()

	maid:task(function()
		print("Hello world!")
	end)

	maid:clean() -- Hello world!
	```

	@return nil
]=]
function Maid:clean(): nil
	for taskId, _task: MaidTask in pairs(self._tasks) do
		if typeof(_task) == "function" then
			_task() -- Run cleaning _task
		elseif typeof(_task) == "RBXScriptConnection" and _task.Connected then
			_task:Disconnect()
		elseif typeof(_task) == "Instance" or (_task and _task.Destroy) then
			_task:Destroy()
		elseif _task and _task.destroy then
			_task:destroy()
		elseif typeof(_task) == "table" then
			if _task.destructor then
				_task.destructor(_task.task)
			end
		end

		self._tasks[taskId] = nil
	end

	return
end

--[=[
	Destroys the Maid instance.

	```lua
	local maid = Maid.new()

	maid:task(function()
		print("Hello world!")
	end)

	maid:destroy()

	maid:clean() -- method no longer exists
	```

	@return nil
]=]
function Maid:destroy(): nil
	for key: any, _ in pairs(self) do
		self[key] = nil
	end

	return nil
end

return Maid
