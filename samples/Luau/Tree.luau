--!optimize 2
--!strict
--!native

--// Tree v1.1.0
--// Authored by @sleitnick and modified by @robloxiandemo
--// Fetched from (https://github.com/Sleitnick/RbxUtil/blob/main/modules/tree/init.lua)
--// Licensed under the MIT License (https://github.com/Sleitnick/RbxUtil/blob/main/LICENSE.md)

local DELIM = "/"

local function FullNameToPath(instance: Instance): string
	return instance:GetFullName():gsub("%.", DELIM)
end

--[=[
	@class Tree
]=]
local Tree = {}

--[=[
	Similar to FindFirstChild, with a few key differences:
	- An error is thrown if the instance is not found
	- A path to the instance can be provided, delimited by forward slashes (e.g. `Path/To/Child`)
	- Optionally, the instance's type can be asserted using `IsA`

	```lua
	-- Find "Child" directly under parent:
	local instance = Tree.Find(parent, "Child")

	-- Find "Child" descendant:
	local instance = Tree.Find(parent, "Path/To/Child")

	-- Find "Child" descendant and assert that it's a BasePart:
	local instance = Tree.Find(parent, "Path/To/Child", "BasePart") :: BasePart
	```
]=]
function Tree.Find(parent: Instance, path: string, assertIsA: string?): Instance
	local instance = parent
	local paths = path:split(DELIM)

	for _, p in paths do
		-- Error for empty path parts:
		if p == "" then
			error(`Invalid path: {path}`, 2)
		end

		instance = instance:FindFirstChild(p)

		-- Error if instance is not found:
		if instance == nil then
			error(`Failed to find {path} in {FullNameToPath(parent)}`, 2)
		end
	end

	-- Assert class type if argument is supplied:
	if assertIsA and not instance:IsA(assertIsA) then
		error(`Got class {instance.ClassName}; expected to be of type {assertIsA}`, 2)
	end

	return instance
end

--[=[
	Returns `true` if the instance is found. Similar to `Tree.Find`, except this returns `true|false`. No error is thrown unless the path is invalid.

	```lua
	-- Check if "Child" exists directly in `parent`:
	if Tree.Exists(parent, "Child") then ... end
	
	-- Check if "Child" descendant exists at `parent.Path.To.Child`:
	if Tree.Exists(parent, "Path/To/Child") then ... end
	
	-- Check if "Child" descendant exists at `parent.Path.To.Child` and is a BasePart:
	if Tree.Exists(parent, "Path/To/Child", "BasePart") then ... end
	```
]=]
function Tree.Exists(parent: Instance, path: string, assertIsA: string?): boolean
	local instance = parent
	local paths = path:split(DELIM)

	for _, p in paths do
		-- Error for empty path parts:
		if p == "" then
			error(`Invalid path: {path}`, 2)
		end

		instance = instance:FindFirstChild(p)

		if instance == nil then
			return false
		end
	end

	if assertIsA and not instance:IsA(assertIsA) then
		return false
	end

	return true
end

--[=[
	@yields
	Waits for the path to exist within the parent instance. Similar to `Tree.Find`, except `WaitForChild`
	is used internally. An optional `timeout` can be supplied, which is passed along to each call to
	`WaitForChild`.

	An error is thrown if the path fails to resolve. This will only happen if the path is invalid _or_ if
	the supplied timeout is reached.

	:::caution Indefinite Yield Possible
	If the `timeout` parameter is not supplied, then the internal call to `WaitForChild` will yield
	indefinitely until the child is found. It is good practice to supply a timeout parameter.
	:::

	```lua
	local child = Tree.Await(parent, "Path/To/Child", 30)
	```
]=]
function Tree.Await(parent: Instance, path: string, timeout: number?, assertIsA: string?): Instance
	local instance = parent
	local paths = path:split(DELIM)

	for _, p in paths do
		-- Error for empty path parts:
		if p == "" then
			error(`Invalid path: {path}`, 2)
		end

		instance = instance:WaitForChild(p, timeout)

		-- Error if instance is not found:
		if instance == nil then
			error(`Failed to await {path} in {FullNameToPath(parent)} (timeout reached)`, 2)
		end
	end

	-- Assert class type if argument is supplied:
	if assertIsA and not instance:IsA(assertIsA) then
		error(`Got class {instance.ClassName}; expected to be of type {assertIsA}`, 2)
	end

	return instance
end

return Tree
