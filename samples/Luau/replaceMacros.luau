--// Authored by @grilme99 (https://github.com/grilme99)
--// Fetched from (https://github.com/grilme99/Flow/blob/973ac39fe52de9ec9407d7ae3d9ab62867b9e982/scripts/replaceMacros.luau)
--// Licensed under the MIT License (https://github.com/grilme99/Flow/blob/973ac39fe52de9ec9407d7ae3d9ab62867b9e982/LICENSE-Brooke)

-- upstream: https://github.com/dead/typeflex/blob/422cb26/tools/repalce_macros.py

local function YG_NODE_STYLE_PROPERTY_SETTER_IMPL(type, name, paramName, instanceName)
	local ret = [[
local function YGNodeStyleSet##name(node: YGNode, paramName: type)
	if node:getStyle().instanceName ~= paramName then
		local style: YGStyle = node:getStyle()
		style.instanceName = paramName
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##name = YGNodeStyleSet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_PROPERTY_SETTER_UNIT_IMPL(type, name, paramName, instanceName)
	local ret = [[
local function YGNodeStyleSet##name(node: YGNode, paramName: type)
	local value: YGValue = YGValue.new(
		YGFloatSanitize(paramName),
		if YGFloatIsUndefined(paramName) then YGUnit.Undefined else YGUnit.Point
	)

	if
		(node:getStyle().instanceName.value ~= value.value and value.unit ~= YGUnit.Undefined)
		or node:getStyle().instanceName.unit ~= value.unit
	then
		local style: YGStyle = node:getStyle()
		style.instanceName = value
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##name = YGNodeStyleSet##name

local function YGNodeStyleSet##namePercent(node: YGNode, paramName: type)
	local value: YGValue = YGValue.new(
		YGFloatSanitize(paramName),
		if YGFloatIsUndefined(paramName) then YGUnit.Undefined else YGUnit.Percent
	)

	if
		(node:getStyle().instanceName.value ~= value.value and value.unit ~= YGUnit.Undefined)
		or node:getStyle().instanceName.unit ~= value.unit
	then
		local style: YGStyle = node:getStyle()
		style.instanceName = value
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##namePercent = YGNodeStyleSet##namePercent

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_PROPERTY_SETTER_UNIT_AUTO_IMPL(type, name, paramName, instanceName)
	local ret = [[
local function YGNodeStyleSet##name(node: YGNode, paramName: type)
	local value: YGValue = YGValue.new(
		YGFloatSanitize(paramName),
		if YGFloatIsUndefined(paramName) then YGUnit.Undefined else YGUnit.Point
	)

	if
		(node:getStyle().instanceName.value ~= value.value and value.unit ~= YGUnit.Undefined)
		or node:getStyle().instanceName.unit ~= value.unit
	then
		local style: YGStyle = node:getStyle()
		style.instanceName = value
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##name = YGNodeStyleSet##name

local function YGNodeStyleSet##namePercent(node: YGNode, paramName: type)
	if
		node:getStyle().instanceName.value ~= YGFloatSanitize(paramName)
		or node:getStyle().instanceName.unit ~= YGUnit.Percent
	then
		local style: YGStyle = node:getStyle()
		style.instanceName.value = YGFloatSanitize(paramName)
		style.instanceName.unit = if YGFloatIsUndefined(paramName) then YGUnit.Auto else YGUnit.Percent
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##namePercent = YGNodeStyleSet##namePercent

local function YGNodeStyleSet##nameAuto(node: YGNode)
	if node:getStyle().instanceName.unit ~= YGUnit.Auto then
		local style: YGStyle = node:getStyle()
		style.instanceName.value = 0
		style.instanceName.unit = YGUnit.Auto
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##nameAuto = YGNodeStyleSet##nameAuto

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_PROPERTY_IMPL(type, name, paramName, instanceName)
	local ret = YG_NODE_STYLE_PROPERTY_SETTER_IMPL(type, name, paramName, instanceName)
	ret ..= [[
local function YGNodeStyleGet##name(node: YGNode): type
	return node:getStyle().instanceName
end
exports.YGNodeStyleGet##name = YGNodeStyleGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_EDGE_PROPERTY_UNIT_IMPL(type, name, paramName, instanceName)
	local ret = [[
local function YGNodeStyleSet##name(node: YGNode, edge: YGEdge, paramName: number)
	local value: YGValue = YGValue.new(
		YGFloatSanitize(paramName),
		if YGFloatIsUndefined(paramName) then YGUnit.Undefined else YGUnit.Point
	)

	if
		(node:getStyle().instanceName[edge].value ~= value.value and value.unit ~= YGUnit.Undefined)
		or node:getStyle().instanceName[edge].unit ~= value.unit
	then
		local style: YGStyle = node:getStyle()
		style.instanceName[edge] = value
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##name = YGNodeStyleSet##name

local function YGNodeStyleSet##namePercent(node: YGNode, edge: YGEdge, paramName: number)
	local value: YGValue = YGValue.new(
		YGFloatSanitize(paramName),
		if YGFloatIsUndefined(paramName) then YGUnit.Undefined else YGUnit.Percent
	)

	if
		(node:getStyle().instanceName[edge].value ~= value.value and value.unit ~= YGUnit.Undefined)
		or node:getStyle().instanceName[edge].unit ~= value.unit
	then
		local style: YGStyle = node:getStyle()
		style.instanceName[edge] = value
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##namePercent = YGNodeStyleSet##namePercent

local function YGNodeStyleGet##name(node: YGNode, edge: YGEdge): type
	local value: YGValue = node:getStyle().instanceName[edge]
	if value.unit == YGUnit.Undefined or value.unit == YGUnit.Auto then
		value.value = YGUndefined
	end

	return value
end
exports.YGNodeStyleGet##name = YGNodeStyleGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_EDGE_PROPERTY_UNIT_AUTO_IMPL(type, name, instanceName)
	local ret = [[
local function YGNodeStyleSet##nameAuto(node: YGNode, edge: YGEdge)
	if node:getStyle().instanceName[edge].unit ~= YGUnit.Auto then
		local style: YGStyle = node:getStyle()
		style.instanceName[edge].value = 0
		style.instanceName[edge].unit = YGUnit.Auto
		node:setStyle(style)
		node:markDirtyAndPropogate()
	end
end
exports.YGNodeStyleSet##nameAuto = YGNodeStyleSet##nameAuto

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_PROPERTY_UNIT_AUTO_IMPL(type, name, paramName, instanceName)
	local ret = YG_NODE_STYLE_PROPERTY_SETTER_UNIT_AUTO_IMPL("number", name, paramName, instanceName)
	ret ..= [[
local function YGNodeStyleGet##name(node: YGNode): type
	local value: YGValue = node:getStyle().instanceName
	if value.unit == YGUnit.Undefined or value.unit == YGUnit.Auto then
		value.value = YGUndefined
	end
	return value
end
exports.YGNodeStyleGet##name = YGNodeStyleGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_STYLE_PROPERTY_UNIT_IMPL(type, name, paramName, instanceName)
	local ret = YG_NODE_STYLE_PROPERTY_SETTER_UNIT_IMPL("number", name, paramName, instanceName)
	ret ..= [[
local function YGNodeStyleGet##name(node: YGNode): type
	local value: YGValue = node:getStyle().instanceName
	if value.unit == YGUnit.Undefined or value.unit == YGUnit.Auto then
		value.value = YGUndefined
	end
	return value
end
exports.YGNodeStyleGet##name = YGNodeStyleGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("paramName", paramName):gsub("instanceName", instanceName)
end

local function YG_NODE_LAYOUT_PROPERTY_IMPL(type, name, instanceName)
	local ret = [[
local function YGNodeLayoutGet##name(node: YGNode): type
	return node:getLayout().instanceName
end
exports.YGNodeLayoutGet##name = YGNodeLayoutGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("instanceName", instanceName)
end

local function YG_NODE_LAYOUT_RESOLVED_PROPERTY_IMPL(type, name, instanceName)
	local ret = [[
local function YGNodeLayoutGet##name(node: YGNode, edge: YGEdge): type
	-- YGAssertWithNode(node, edge <= YGEdge.End, "Cannot get layout properties of multi-edge shorthands")

	if edge == YGEdge.Start then
		if node:getLayout().direction == YGDirection.RTL then
			return node:getLayout().instanceName[YGEdge.Right]
		else
			return node:getLayout().instanceName[YGEdge.Left]
		end
	end

	if edge == YGEdge.End then
		if node:getLayout().direction == YGDirection.RTL then
			return node:getLayout().instanceName[YGEdge.Left]
		else
			return node:getLayout().instanceName[YGEdge.Right]
		end
	end

	return node:getLayout().instanceName[edge]
end
exports.YGNodeLayoutGet##name = YGNodeLayoutGet##name

]]

	return ret:gsub("type", type):gsub("##name", name):gsub("instanceName", instanceName)
end

local cod = ""

cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGDirection", "Direction", "direction", "direction")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGFlexDirection", "FlexDirection", "flexDirection", "flexDirection")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGJustify", "JustifyContent", "justifyContent", "justifyContent")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGAlign", "AlignContent", "alignContent", "alignContent")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGAlign", "AlignItems", "alignItems", "alignItems")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGAlign", "AlignSelf", "alignSelf", "alignSelf")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGPositionType", "PositionType", "positionType", "positionType")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGWrap", "FlexWrap", "flexWrap", "flexWrap")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGOverflow", "Overflow", "overflow", "overflow")
cod ..= YG_NODE_STYLE_PROPERTY_IMPL("YGDisplay", "Display", "display", "display")
cod ..= YG_NODE_STYLE_EDGE_PROPERTY_UNIT_IMPL("YGValue", "Position", "position", "position")
cod ..= YG_NODE_STYLE_EDGE_PROPERTY_UNIT_IMPL("YGValue", "Margin", "margin", "margin")
cod ..= YG_NODE_STYLE_EDGE_PROPERTY_UNIT_IMPL("YGValue", "Padding", "padding", "padding")
cod ..= YG_NODE_STYLE_EDGE_PROPERTY_UNIT_AUTO_IMPL("YGValue", "Margin", "margin")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_AUTO_IMPL("YGValue", "Width", "width", "dimensions[YGDimension.Width]")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_AUTO_IMPL("YGValue", "Height", "height", "dimensions[YGDimension.Height]")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_IMPL("YGValue", "MinWidth", "minWidth", "minDimensions[YGDimension.Width]")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_IMPL("YGValue", "MinHeight", "minHeight", "minDimensions[YGDimension.Height]")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_IMPL("YGValue", "MaxWidth", "maxWidth", "maxDimensions[YGDimension.Width]")
cod ..= YG_NODE_STYLE_PROPERTY_UNIT_IMPL("YGValue", "MaxHeight", "maxHeight", "maxDimensions[YGDimension.Height]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Left", "position[YGEdge.Left]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Top", "position[YGEdge.Top]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Right", "position[YGEdge.Right]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Bottom", "position[YGEdge.Bottom]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Width", "dimensions[YGDimension.Width]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("number", "Height", "dimensions[YGDimension.Height]")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("YGDirection", "Direction", "direction")
cod ..= YG_NODE_LAYOUT_PROPERTY_IMPL("boolean", "HadOverflow", "hadOverflow")
cod ..= YG_NODE_LAYOUT_RESOLVED_PROPERTY_IMPL("number", "Margin", "margin")
cod ..= YG_NODE_LAYOUT_RESOLVED_PROPERTY_IMPL("number", "Border", "border")
cod ..= YG_NODE_LAYOUT_RESOLVED_PROPERTY_IMPL("number", "Padding", "padding")

print(cod)