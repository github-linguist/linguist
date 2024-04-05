--// Authored by @Filiptibell (https://github.com/filiptibell)
--// Fetched from (https://github.com/lune-org/lune/blob/main/types/roblox.luau)
--// Licensed under the Mozilla Public License v2.0 (https://github.com/lune-org/lune/blob/main/LICENSE.txt)

--!strict
export type DatabaseScriptability = "None" | "Custom" | "Read" | "ReadWrite" | "Write"

export type DatabasePropertyTag =
	"Deprecated"
	| "Hidden"
	| "NotBrowsable"
	| "NotReplicated"
	| "NotScriptable"
	| "ReadOnly"
	| "WriteOnly"

export type DatabaseClassTag =
	"Deprecated"
	| "NotBrowsable"
	| "NotCreatable"
	| "NotReplicated"
	| "PlayerReplicated"
	| "Service"
	| "Settings"
	| "UserSettings"

export type DatabaseProperty = {
	--[=[
		The name of the property.
	]=]
	Name: string,
	--[=[
		The datatype of the property.

		For normal datatypes this will be a string such as `string`, `Color3`, ...

		For enums this will be a string formatted as `Enum.EnumName`.
	]=]
	Datatype: string,
	--[=[
		The scriptability of this property, meaning if it can be written / read at runtime.

		All properties are writable and readable in Lune even if scriptability is not.
	]=]
	Scriptability: DatabaseScriptability,
	--[=[
		Tags describing the property.

		These include information such as if the property can be replicated to players
		at runtime, if the property should be hidden in Roblox Studio, and more.
	]=]
	Tags: { DatabasePropertyTag },
}

export type DatabaseClass = {
	--[=[
		The name of the class.
	]=]
	Name: string,
	--[=[
		The superclass (parent class) of this class.

		May be nil if no parent class exists.
	]=]
	Superclass: string?,
	--[=[
		Known properties for this class.
	]=]
	Properties: { [string]: DatabaseProperty },
	--[=[
		Default values for properties of this class.

		Note that these default properties use Lune's built-in datatype
		userdatas, and that if there is a new datatype that Lune does
		not yet know about, it may be missing from this table.
	]=]
	DefaultProperties: { [string]: any },
	--[=[
		Tags describing the class.

		These include information such as if the class can be replicated
		to players at runtime, and top-level class categories.
	]=]
	Tags: { DatabaseClassTag },
}

export type DatabaseEnum = {
	--[=[
		The name of this enum, for example `PartType` or `UserInputState`.
	]=]
	Name: string,
	--[=[
		Members of this enum.

		Note that this is a direct map of name -> enum values,
		and does not actually use the EnumItem datatype itself.
	]=]
	Items: { [string]: number },
}

export type Database = {
	--[=[
		The current version of the reflection database.

		This will follow the format `x.y.z.w`, which most commonly looks something like `0.567.0.123456789`
	]=]
	Version: string,
	--[=[
		Retrieves a list of all currently known class names.
	]=]
	GetClassNames: (self: Database) -> { string },
	--[=[
		Retrieves a list of all currently known enum names.
	]=]
	GetEnumNames: (self: Database) -> { string },
	--[=[
		Gets a class with the exact given name, if one exists.
	]=]
	GetClass: (self: Database, name: string) -> DatabaseClass?,
	--[=[
		Gets an enum with the exact given name, if one exists.
	]=]
	GetEnum: (self: Database, name: string) -> DatabaseEnum?,
	--[=[
		Finds a class with the given name.

		This will use case-insensitive matching and ignore leading and trailing whitespace.
	]=]
	FindClass: (self: Database, name: string) -> DatabaseClass?,
	--[=[
		Finds an enum with the given name.

		This will use case-insensitive matching and ignore leading and trailing whitespace.
	]=]
	FindEnum: (self: Database, name: string) -> DatabaseEnum?,
}

type InstanceProperties = {
	Parent: Instance?,
	ClassName: string,
	Name: string,
	-- FIXME: This breaks intellisense, but we need some way to access
	-- instance properties without casting the entire instance to any...
	-- [string]: any,
}

type InstanceMetatable = {
	Clone: (self: Instance) -> Instance,
	Destroy: (self: Instance) -> (),
	ClearAllChildren: (self: Instance) -> (),

	GetChildren: (self: Instance) -> { Instance },
	GetDebugId: (self: Instance) -> string,
	GetDescendants: (self: Instance) -> { Instance },
	GetFullName: (self: Instance) -> string,

	FindFirstAncestor: (self: Instance, name: string) -> Instance?,
	FindFirstAncestorOfClass: (self: Instance, className: string) -> Instance?,
	FindFirstAncestorWhichIsA: (self: Instance, className: string) -> Instance?,
	FindFirstChild: (self: Instance, name: string, recursive: boolean?) -> Instance?,
	FindFirstChildOfClass: (self: Instance, className: string, recursive: boolean?) -> Instance?,
	FindFirstChildWhichIsA: (self: Instance, className: string, recursive: boolean?) -> Instance?,

	IsA: (self: Instance, className: string) -> boolean,
	IsAncestorOf: (self: Instance, descendant: Instance) -> boolean,
	IsDescendantOf: (self: Instance, ancestor: Instance) -> boolean,

	GetAttribute: (self: Instance, name: string) -> any,
	GetAttributes: (self: Instance) -> { [string]: any },
	SetAttribute: (self: Instance, name: string, value: any) -> (),

	GetTags: (self: Instance) -> { string },
	HasTag: (self: Instance, name: string) -> boolean,
	AddTag: (self: Instance, name: string) -> (),
	RemoveTag: (self: Instance, name: string) -> (),
}

export type Instance = typeof(setmetatable(
	(nil :: any) :: InstanceProperties,
	(nil :: any) :: { __index: InstanceMetatable }
))

export type DataModelProperties = {}
export type DataModelMetatable = {
	GetService: (self: DataModel, name: string) -> Instance,
	FindService: (self: DataModel, name: string) -> Instance?,
}

export type DataModel =
	Instance
	& typeof(setmetatable(
		(nil :: any) :: DataModelProperties,
		(nil :: any) :: { __index: DataModelMetatable }
	))

--[=[
	@class Roblox

	Built-in library for manipulating Roblox place & model files

	### Example usage

	```lua
	local fs = require("@lune/fs")
	local roblox = require("@lune/roblox")

	-- Reading a place file
	local placeFile = fs.readFile("myPlaceFile.rbxl")
	local game = roblox.deserializePlace(placeFile)

	-- Manipulating and reading instances - just like in Roblox!
	local workspace = game:GetService("Workspace")
	for _, child in workspace:GetChildren() do
		print("Found child " .. child.Name .. " of class " .. child.ClassName)
	end

	-- Writing a place file
	local newPlaceFile = roblox.serializePlace(game)
	fs.writeFile("myPlaceFile.rbxl", newPlaceFile)
	```
]=]
local roblox = {}

--[=[
	@within Roblox
	@tag must_use

	Deserializes a place into a DataModel instance.

	This function accepts a string of contents, *not* a file path.
	If reading a place file from a file path is desired, `fs.readFile`
	can be used and the resulting string may be passed to this function.

	### Example usage

	```lua
	local fs = require("@lune/fs")
	local roblox = require("@lune/roblox")

	local placeFile = fs.readFile("filePath.rbxl")
	local game = roblox.deserializePlace(placeFile)
	```

	@param contents The contents of the place to read
]=]
function roblox.deserializePlace(contents: string): DataModel
	return nil :: any
end

--[=[
	@within Roblox
	@tag must_use

	Deserializes a model into an array of instances.

	This function accepts a string of contents, *not* a file path.
	If reading a model file from a file path is desired, `fs.readFile`
	can be used and the resulting string may be passed to this function.

	### Example usage

	```lua
	local fs = require("@lune/fs")
	local roblox = require("@lune/roblox")

	local modelFile = fs.readFile("filePath.rbxm")
	local instances = roblox.deserializeModel(modelFile)
	```

	@param contents The contents of the model to read
]=]
function roblox.deserializeModel(contents: string): { Instance }
	return nil :: any
end

--[=[
	@within Roblox
	@tag must_use

	Serializes a place from a DataModel instance.

	This string can then be written to a file, or sent over the network.

	### Example usage

	```lua
	local fs = require("@lune/fs")
	local roblox = require("@lune/roblox")

	local placeFile = roblox.serializePlace(game)
	fs.writeFile("filePath.rbxl", placeFile)
	```

	@param dataModel The DataModel for the place to serialize
	@param xml If the place should be serialized as xml or not. Defaults to `false`, meaning the place gets serialized using the binary format and not xml.
]=]
function roblox.serializePlace(dataModel: DataModel, xml: boolean?): string
	return nil :: any
end

--[=[
	@within Roblox
	@tag must_use

	Serializes one or more instances as a model.

	This string can then be written to a file, or sent over the network.

	### Example usage

	```lua
	local fs = require("@lune/fs")
	local roblox = require("@lune/roblox")

	local modelFile = roblox.serializeModel({ instance1, instance2, ... })
	fs.writeFile("filePath.rbxm", modelFile)
	```

	@param instances The array of instances to serialize
	@param xml If the model should be serialized as xml or not. Defaults to `false`, meaning the model gets serialized using the binary format and not xml.
]=]
function roblox.serializeModel(instances: { Instance }, xml: boolean?): string
	return nil :: any
end

--[=[
	@within Roblox
	@tag must_use

	Gets the current auth cookie, for usage with Roblox web APIs.

	Note that this auth cookie is formatted for use as a "Cookie" header,
	and that it contains restrictions so that it may only be used for
	official Roblox endpoints. To get the raw cookie value without any
	additional formatting, you can pass `true` as the first and only parameter.

	### Example usage

	```lua
	local roblox = require("@lune/roblox")
	local net = require("@lune/net")

	local cookie = roblox.getAuthCookie()
	assert(cookie ~= nil, "Failed to get roblox auth cookie")

	local myPrivatePlaceId = 1234567890

	local response = net.request({
		url = "https://assetdelivery.roblox.com/v2/assetId/" .. tostring(myPrivatePlaceId),
		headers = {
			Cookie = cookie,
		},
	})

	local responseTable = net.jsonDecode(response.body)
	local responseLocation = responseTable.locations[1].location
	print("Download link to place: " .. responseLocation)
	```

	@param raw If the cookie should be returned as a pure value or not. Defaults to false
]=]
function roblox.getAuthCookie(raw: boolean?): string?
	return nil :: any
end

--[=[
	@within Roblox
	@tag must_use

	Gets the bundled reflection database.

	This database contains information about Roblox enums, classes, and their properties.

	### Example usage

	```lua
	local roblox = require("@lune/roblox")

	local db = roblox.getReflectionDatabase()

	print("There are", #db:GetClassNames(), "classes in the reflection database")

	print("All base instance properties:")

	local class = db:GetClass("Instance")
	for name, prop in class.Properties do
		print(string.format(
			"- %s with datatype %s and default value %s",
			prop.Name,
			prop.Datatype,
			tostring(class.DefaultProperties[prop.Name])
		))
	end
	```
]=]
function roblox.getReflectionDatabase(): Database
	return nil :: any
end

--[=[
	@within Roblox

	Implements a property for all instances of the given `className`.

	This takes into account class hierarchies, so implementing a property
	for the `BasePart` class will also implement it for `Part` and others,
	unless a more specific implementation is added to the `Part` class directly.

	### Behavior

	The given `getter` callback will be called each time the property is
	indexed, with the instance as its one and only argument. The `setter`
	callback, if given, will be called each time the property should be set,
	with the instance as the first argument and the property value as second.

	### Example usage

	```lua
	local roblox = require("@lune/roblox")

	local part = roblox.Instance.new("Part")

	local propertyValues = {}
	roblox.implementProperty(
		"BasePart",
		"CoolProp",
		function(instance)
			if propertyValues[instance] == nil then
				propertyValues[instance] = 0
			end
			propertyValues[instance] += 1
			return propertyValues[instance]
		end,
		function(instance, value)
			propertyValues[instance] = value
		end
	)

	print(part.CoolProp) --> 1
	print(part.CoolProp) --> 2
	print(part.CoolProp) --> 3

	part.CoolProp = 10

	print(part.CoolProp) --> 11
	print(part.CoolProp) --> 12
	print(part.CoolProp) --> 13
	```

	@param className The class to implement the property for.
	@param propertyName The name of the property to implement.
	@param getter The function which will be called to get the property value when indexed.
	@param setter The function which will be called to set the property value when indexed. Defaults to a function that will error with a message saying the property is read-only.
]=]
function roblox.implementProperty<T>(
	className: string,
	propertyName: string,
	getter: (instance: Instance) -> T,
	setter: ((instance: Instance, value: T) -> ())?
)
	return nil :: any
end

--[=[
	@within Roblox

	Implements a method for all instances of the given `className`.

	This takes into account class hierarchies, so implementing a method
	for the `BasePart` class will also implement it for `Part` and others,
	unless a more specific implementation is added to the `Part` class directly.

	### Behavior

	The given `callback` will be called every time the method is called,
	and will receive the instance it was called on as its first argument.
	The remaining arguments will be what the caller passed to the method, and
	all values returned from the callback will then be returned to the caller.

	### Example usage

	```lua
	local roblox = require("@lune/roblox")

	local part = roblox.Instance.new("Part")

	roblox.implementMethod("BasePart", "TestMethod", function(instance, ...)
	    print("Called TestMethod on instance", instance, "with", ...)
	end)

	part:TestMethod("Hello", "world!")
	--> Called TestMethod on instance Part with Hello, world!
	```

	@param className The class to implement the method for.
	@param methodName The name of the method to implement.
	@param callback The function which will be called when the method is called.
]=]
function roblox.implementMethod(
	className: string,
	methodName: string,
	callback: (instance: Instance, ...any) -> ...any
)
	return nil :: any
end

-- TODO: Make typedefs for all of the datatypes as well...
roblox.Instance = (nil :: any) :: {
	new: ((className: "DataModel") -> DataModel) & ((className: string) -> Instance),
}

return roblox
