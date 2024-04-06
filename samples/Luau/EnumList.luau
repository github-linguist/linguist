--// Authored by @Sleitnick (https://github.com/sleitnick)
--// Fetched from (https://github.com/Sleitnick/RbxUtil/blob/main/modules/enum-list/init.lua)
--// Licensed under the MIT License (https://github.com/Sleitnick/RbxUtil/blob/main/LICENSE.md)

--!optimize 2
--!strict
--!native

-- EnumList
-- Stephen Leitnick
-- January 08, 2021

type EnumNames = { string }

--[=[
	@interface EnumItem
	.Name string
	.Value number
	.EnumType EnumList
	@within EnumList
]=]
export type EnumItem = {
	Name: string,
	Value: number,
	EnumType: any,
}

local LIST_KEY = newproxy()
local NAME_KEY = newproxy()

local function CreateEnumItem(name: string, value: number, enum: any): EnumItem
	local enumItem = {
		Name = name,
		Value = value,
		EnumType = enum,
	}
	table.freeze(enumItem)
	return enumItem
end

--[=[
	@class EnumList
	Defines a new Enum.
]=]
local EnumList = {}
EnumList.__index = EnumList

--[=[
	@param name string
	@param enums {string}
	@return EnumList
	Constructs a new EnumList.

	```lua
	local directions = EnumList.new("Directions", {
		"Up",
		"Down",
		"Left",
		"Right",
	})

	local direction = directions.Up
	```
]=]
function EnumList.new(name: string, enums: EnumNames)
	assert(type(name) == "string", "Name string required")
	assert(type(enums) == "table", "Enums table required")
	local self = {}
	self[LIST_KEY] = {}
	self[NAME_KEY] = name
	for i, enumName in ipairs(enums) do
		assert(type(enumName) == "string", "Enum name must be a string")
		local enumItem = CreateEnumItem(enumName, i, self)
		self[enumName] = enumItem
		table.insert(self[LIST_KEY], enumItem)
	end
	return table.freeze(setmetatable(self, EnumList))
end

--[=[
	@param obj any
	@return boolean
	Returns `true` if `obj` belongs to the EnumList.
]=]
function EnumList:BelongsTo(obj: any): boolean
	return type(obj) == "table" and obj.EnumType == self
end

--[=[
	Returns an array of all enum items.
	@return {EnumItem}
	@since v2.0.0
]=]
function EnumList:GetEnumItems()
	return self[LIST_KEY]
end

--[=[
	Get the name of the enum.
	@return string
	@since v2.0.0
]=]
function EnumList:GetName()
	return self[NAME_KEY]
end

export type EnumList = typeof(EnumList.new(...))

return EnumList
