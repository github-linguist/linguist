--!optimize 2
--!strict
--!native

--// Ser v1.0.5
--// Authored by @sleitnick and modified by @robloxiandemo
--// Fetched from (https://github.com/Sleitnick/RbxUtil/blob/main/modules/ser/init.lua)
--// Licensed under the MIT License (https://github.com/Sleitnick/RbxUtil/blob/main/LICENSE.md)

--[[

	Ser is a serialization/deserialization utility module that is used
	by Knit to automatically serialize/deserialize values passing
	through remote functions and remote events.


	Ser.Classes = {
		[ClassName] = {
			Serialize = (value) -> serializedValue
			Deserialize = (value) => deserializedValue
		}
	}

	Ser.SerializeArgs(...)            -> table
	Ser.SerializeArgsAndUnpack(...)   -> Tuple
	Ser.DeserializeArgs(...)          -> table
	Ser.DeserializeArgsAndUnpack(...) -> Tuple
	Ser.Serialize(value: any)         -> any
	Ser.Deserialize(value: any)       -> any
	Ser.UnpackArgs(args: table)       -> Tuple

--]]

type Args = {
	n: number,
	[any]: any,
}

local Option = require("samples/Luau/Option.luau")

--[=[
	@class Ser

	Library for serializing and deserializing data.

	See the `Classes` property for information on extending the use
	of the Ser library to include other classes.

]=]
local Ser = {}

--[=[
	@within Ser
	@prop Classes table

	A dictionary of classes along with a Serialize and Deserialize function.
	For instance, the default class added is the Option class, which looks
	like the following:

	```lua
	Ser.Classes.Option = {
		Serialize = function(opt) return opt:Serialize() end;
		Deserialize = Option.Deserialize;
	}
	```

	Add to this table in order to extend what classes are automatically
	serialized/deserialized.

	The Ser library checks every object's `ClassName` field in both serialized
	and deserialized data in order to map it to the correct function within
	the Classes table.
]=]
Ser.Classes = {
	Option = {
		Serialize = function(opt)
			return opt:Serialize()
		end,
		Deserialize = Option.Deserialize,
	},
}

--[=[
	@param ... any
	@return args: table
	Serializes the arguments and returns the serialized values in a table.
]=]
function Ser.SerializeArgs(...: any): Args
	local args = { (...) }
	for i, arg in ipairs(args) do
		if type(arg) == "table" then
			local ser = Ser.Classes[arg.ClassName]
			if ser then
				args[i] = ser.Serialize(arg)
			end
		end
	end
	return args
end

--[=[
	@param ... any
	@return args: ...any
	Serializes the arguments and returns the serialized values.
]=]
function Ser.SerializeArgsAndUnpack(...: any): ...any
	local args = Ser.SerializeArgs(...)
	return unpack(args, 1, args.n)
end

--[=[
	@param ... any
	@return args: table
	Deserializes the arguments and returns the deserialized values in a table.
]=]
function Ser.DeserializeArgs(...: any): Args
	local args = { (...) }
	for i, arg in ipairs(args) do
		if type(arg) == "table" then
			local ser = Ser.Classes[arg.ClassName]
			if ser then
				args[i] = ser.Deserialize(arg)
			end
		end
	end
	return args
end

--[=[
	@param ... any
	@return args: table
	Deserializes the arguments and returns the deserialized values.
]=]
function Ser.DeserializeArgsAndUnpack(...: any): ...any
	local args = Ser.DeserializeArgs(...)
	return unpack(args, 1, args.n)
end

--[=[
	@param value any
	@return any
	Serializes the given value.
]=]
function Ser.Serialize(value: any): any
	if type(value) == "table" then
		local ser = Ser.Classes[value.ClassName]
		if ser then
			value = ser.Serialize(value)
		end
	end
	return value
end

--[=[
	@param value any
	@return any
	Deserializes the given value.
]=]
function Ser.Deserialize(value: any): any
	if type(value) == "table" then
		local ser = Ser.Classes[value.ClassName]
		if ser then
			value = ser.Deserialize(value)
		end
	end
	return value
end

--[=[
	@param value any
	@return any
	Unpacks the arguments returned by either `SerializeArgs` or `DeserializeArgs`.
]=]
function Ser.UnpackArgs(value: Args): ...any
	return unpack(value, 1, value.n)
end

return Ser
