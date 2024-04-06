--// Authored by @Sleitnick (https://github.com/sleitnick)
--// Fetched from (https://github.com/Sleitnick/RbxUtil/blob/main/modules/wait-for/init.lua)
--// Licensed under the MIT License (https://github.com/Sleitnick/RbxUtil/blob/main/LICENSE.md)

--!optimize 2
--!strict
--!native

-- WaitFor
-- Stephen Leitnick
-- January 17, 2022

local RunService = game:GetService("RunService")

local Promise = require(script.Parent.Promise)

local DEFAULT_TIMEOUT = 60

--[=[
	@class WaitFor
	Utility class for awaiting the existence of instances.

	By default, all promises timeout after 60 seconds, unless the `timeout`
	argument is specified.

	:::note
	Promises will be rejected if the parent (or any ancestor) is unparented
	from the game.
	:::

	:::caution Set name before parent
	When waiting for instances based on name (e.g. `WaitFor.Child`), the `WaitFor`
	system is listening to events to capture these instances being added. This
	means that the name must be set _before_ being parented into the object.
	:::
]=]
local WaitFor = {}

--[=[
	@within WaitFor
	@prop Error {Unparented: string, ParentChanged: string}
]=]
WaitFor.Error = {
	Unparented = "Unparented",
	ParentChanged = "ParentChanged",
}

local function PromiseWatchAncestry(instance: Instance, promise)
	return Promise.race({
		promise,
		Promise.fromEvent(instance.AncestryChanged, function(_, newParent)
			return newParent == nil
		end):andThen(function()
			return Promise.reject(WaitFor.Error.Unparented)
		end),
	})
end

--[=[
	@return Promise<Instance>
	Wait for a child to exist within a given parent based on the child name.

	```lua
	WaitFor.Child(parent, "SomeObject"):andThen(function(someObject)
		print(someObject, "now exists")
	end):catch(warn)
	```
]=]
function WaitFor.Child(parent: Instance, childName: string, timeout: number?)
	local child = parent:FindFirstChild(childName)
	if child then
		return Promise.resolve(child)
	end
	return PromiseWatchAncestry(
		parent,
		Promise.fromEvent(parent.ChildAdded, function(c)
			return c.Name == childName
		end):timeout(timeout or DEFAULT_TIMEOUT)
	)
end

--[=[
	@return Promise<{Instance}>
	Wait for all children to exist within the given parent.

	```lua
	WaitFor.Children(parent, {"SomeObject01", "SomeObject02"}):andThen(function(children)
		local someObject01, someObject02 = table.unpack(children)
	end)
	```

	:::note
	Once all children are found, a second check is made to ensure that all children
	are still directly parented to the given `parent` (since one child's parent
	might have changed before another child was found). A rejected promise with the
	`WaitFor.Error.ParentChanged` error will be thrown if any parents of the children
	no longer match the given `parent`.
	:::
]=]
function WaitFor.Children(parent: Instance, childrenNames: { string }, timeout: number?)
	local all = table.create(#childrenNames)
	for i, childName in ipairs(childrenNames) do
		all[i] = WaitFor.Child(parent, childName, timeout)
	end
	return Promise.all(all):andThen(function(children)
		-- Check that all are still parented
		for _, child in ipairs(children) do
			if child.Parent ~= parent then
				return Promise.reject(WaitFor.Error.ParentChanged)
			end
		end
		return children
	end)
end

--[=[
	@return Promise<Instance>
	Wait for a descendant to exist within a given parent. This is similar to
	`WaitFor.Child`, except it looks for all descendants instead of immediate
	children.

	```lua
	WaitFor.Descendant(parent, "SomeDescendant"):andThen(function(someDescendant)
		print("SomeDescendant now exists")
	end)
	```
]=]
function WaitFor.Descendant(parent: Instance, descendantName: string, timeout: number?)
	local descendant = parent:FindFirstChild(descendantName, true)
	if descendant then
		return Promise.resolve(descendant)
	end
	return PromiseWatchAncestry(
		parent,
		Promise.fromEvent(parent.DescendantAdded, function(d)
			return d.Name == descendantName
		end):timeout(timeout or DEFAULT_TIMEOUT)
	)
end

--[=[
	@return Promise<{Instance}>
	Wait for all descendants to exist within a given parent.

	```lua
	WaitFor.Descendants(parent, {"SomeDescendant01", "SomeDescendant02"}):andThen(function(descendants)
		local someDescendant01, someDescendant02 = table.unpack(descendants)
	end)
	```

	:::note
	Once all descendants are found, a second check is made to ensure that none of the
	instances have moved outside of the parent (since one instance might change before
	another instance is found). A rejected promise with the `WaitFor.Error.ParentChanged`
	error will be thrown if any of the instances are no longer descendants of the given
	`parent`.
	:::
]=]
function WaitFor.Descendants(parent: Instance, descendantNames: { string }, timeout: number?)
	local all = table.create(#descendantNames)
	for i, descendantName in ipairs(descendantNames) do
		all[i] = WaitFor.Descendant(parent, descendantName, timeout)
	end
	return Promise.all(all):andThen(function(descendants)
		-- Check that all are still parented
		for _, descendant in ipairs(descendants) do
			if not descendant:IsDescendantOf(parent) then
				return Promise.reject(WaitFor.Error.ParentChanged)
			end
		end
		return descendants
	end)
end

--[=[
	@return Promise<Instance>
	Wait for the PrimaryPart of a model to exist.

	```lua
	WaitFor.PrimaryPart(model):andThen(function(primaryPart)
		print(primaryPart == model.PrimaryPart)
	end)
	```
]=]
function WaitFor.PrimaryPart(model: Model, timeout: number?)
	local primary = model.PrimaryPart
	if primary then
		return Promise.resolve(primary)
	end
	return PromiseWatchAncestry(
		model,
		Promise.fromEvent(model:GetPropertyChangedSignal("PrimaryPart"), function()
			primary = model.PrimaryPart
			return primary ~= nil
		end)
			:andThen(function()
				return primary
			end)
			:timeout(timeout or DEFAULT_TIMEOUT)
	)
end

--[=[
	@return Promise<Instance>
	Wait for the Value of an ObjectValue to exist.

	```lua
	WaitFor.ObjectValue(someObjectValue):andThen(function(value)
		print("someObjectValue's value is", value)
	end)
	```
]=]
function WaitFor.ObjectValue(objectValue: ObjectValue, timeout: number?)
	local value = objectValue.Value
	if value then
		return Promise.resolve(value)
	end
	return PromiseWatchAncestry(
		objectValue,
		Promise.fromEvent(objectValue.Changed, function(v)
			value = v
			return value ~= nil
		end)
			:andThen(function()
				return value
			end)
			:timeout(timeout or DEFAULT_TIMEOUT)
	)
end

--[=[
	@return Promise<T>
	Wait for the given predicate function to return a non-nil value of
	of type `T`. The predicate is fired every RunService Heartbeat step.

	```lua
	-- Example, waiting for some property to be set:
	WaitFor.Custom(function() return vectorForce.Attachment0 end):andThen(function(a0)
		print(a0)
	end)
	```
]=]
function WaitFor.Custom<T>(predicate: () -> T?, timeout: number?)
	local value = predicate()
	if value ~= nil then
		return Promise.resolve(value)
	end
	return Promise.new(function(resolve, _reject, onCancel)
		local heartbeat
		local function OnDone()
			heartbeat:Disconnect()
		end
		local function Update()
			local v = predicate()
			if v ~= nil then
				OnDone()
				resolve(v)
			end
		end
		heartbeat = RunService.Heartbeat:Connect(Update)
		onCancel(OnDone)
	end):timeout(timeout or DEFAULT_TIMEOUT)
end

return WaitFor
