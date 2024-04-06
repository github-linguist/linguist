--// Authored by @JSDotLua (https://github.com/jsdotlua)
--// Fetched from (https://github.com/jsdotlua/react-lua/blob/main/modules/react-roblox/src/client/ReactRobloxHostConfig.lua)
--// Licensed under the MIT License (https://github.com/jsdotlua/react-lua/blob/main/LICENSE)

--!strict
-- ROBLOX upstream: https://github.com/facebook/react/blob/8e5adfbd7e605bda9c5e96c10e015b3dc0df688e/packages/react-dom/src/client/ReactDOMHostConfig.js
-- ROBLOX upstream: https://github.com/facebook/react/blob/efd8f6442d1aa7c4566fe812cba03e7e83aaccc3/packages/react-native-renderer/src/ReactNativeHostConfig.js
--[[*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
]]
-- FIXME (roblox): remove this when our unimplemented
local function unimplemented(message: string)
	print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
	print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
	print("UNIMPLEMENTED ERROR: " .. tostring(message))
	error("FIXME (roblox): " .. message .. " is unimplemented", 2)
end

local CollectionService = game:GetService("CollectionService")
local LuauPolyfill = require("@pkg/@jsdotlua/luau-polyfill")
local inspect = LuauPolyfill.util.inspect
local console = require("@pkg/@jsdotlua/shared").console
local Object = LuauPolyfill.Object
local setTimeout = LuauPolyfill.setTimeout
local clearTimeout = LuauPolyfill.clearTimeout

-- local type {DOMEventName} = require(Packages.../events/DOMEventNames')
-- local type {Fiber, FiberRoot} = require(Packages.react-reconciler/src/ReactInternalTypes')
-- local type {
--   BoundingRect,
--   IntersectionObserverOptions,
--   ObserveVisibleRectsCallback,
-- } = require(Packages.react-reconciler/src/ReactTestSelectors')
local ReactRobloxHostTypes = require("./ReactRobloxHostTypes.roblox.lua")
type RootType = ReactRobloxHostTypes.RootType
type Container = ReactRobloxHostTypes.Container
type HostInstance = ReactRobloxHostTypes.HostInstance
type SuspenseInstance = ReactRobloxHostTypes.SuspenseInstance
type TextInstance = ReactRobloxHostTypes.TextInstance
type Props = ReactRobloxHostTypes.Props
type Type = ReactRobloxHostTypes.Type
type HostContext = ReactRobloxHostTypes.HostContext

-- local type {ReactScopeInstance} = require(Packages.shared/ReactTypes')
-- local type {ReactDOMFundamentalComponentInstance} = require(Packages.../shared/ReactDOMTypes')

local ReactRobloxComponentTree = require("./ReactRobloxComponentTree")
local precacheFiberNode = ReactRobloxComponentTree.precacheFiberNode
local uncacheFiberNode = ReactRobloxComponentTree.uncacheFiberNode
local updateFiberProps = ReactRobloxComponentTree.updateFiberProps
-- local getClosestInstanceFromNode = ReactRobloxComponentTree.getClosestInstanceFromNode
-- local getFiberFromScopeInstance = ReactRobloxComponentTree.getFiberFromScopeInstance
-- local getInstanceFromNodeDOMTree = ReactRobloxComponentTree.getInstanceFromNode
-- local isContainerMarkedAsRoot = ReactRobloxComponentTree.isContainerMarkedAsRoot

-- local {hasRole} = require(Packages../DOMAccessibilityRoles')
local ReactRobloxComponent = require("./ReactRobloxComponent")
-- local createElement = ReactRobloxComponent.createElement
-- local createTextNode = ReactRobloxComponent.createTextNode
local setInitialProperties = ReactRobloxComponent.setInitialProperties
local diffProperties = ReactRobloxComponent.diffProperties
local updateProperties = ReactRobloxComponent.updateProperties
local cleanupHostComponent = ReactRobloxComponent.cleanupHostComponent
-- local diffHydratedProperties = ReactRobloxComponent.diffHydratedProperties
-- local diffHydratedText = ReactRobloxComponent.diffHydratedText
-- local trapClickOnNonInteractiveElement = ReactRobloxComponent.trapClickOnNonInteractiveElement
-- local warnForUnmatchedText = ReactRobloxComponent.warnForUnmatchedText
-- local warnForDeletedHydratableElement = ReactRobloxComponent.warnForDeletedHydratableElement
-- local warnForDeletedHydratableText = ReactRobloxComponent.warnForDeletedHydratableText
-- local warnForInsertedHydratedElement = ReactRobloxComponent.warnForInsertedHydratedElement
-- local warnForInsertedHydratedText = ReactRobloxComponent.warnForInsertedHydratedText
-- local {getSelectionInformation, restoreSelection} = require(Packages../ReactInputSelection')
-- local setTextContent = require(Packages../setTextContent')
-- local {validateDOMNesting, updatedAncestorInfo} = require(Packages../validateDOMNesting')
-- local {
--   isEnabled as ReactBrowserEventEmitterIsEnabled,
--   setEnabled as ReactBrowserEventEmitterSetEnabled,
-- } = require(Packages.../events/ReactDOMEventListener')
-- local {getChildNamespace} = require(Packages.../shared/DOMNamespaces')
-- local {
--   ELEMENT_NODE,
--   TEXT_NODE,
--   COMMENT_NODE,
--   DOCUMENT_NODE,
--   DOCUMENT_FRAGMENT_NODE,
-- } = require(Packages.../shared/HTMLNodeType')
-- local dangerousStyleValue = require(Packages.../shared/dangerousStyleValue')

-- local {REACT_OPAQUE_ID_TYPE} = require(Packages.shared/ReactSymbols')
-- local {retryIfBlockedOn} = require(Packages.../events/ReactDOMEventReplaying')

local ReactFeatureFlags = require("@pkg/@jsdotlua/shared").ReactFeatureFlags
-- local enableSuspenseServerRenderer = ReactFeatureFlags.enableSuspenseServerRenderer
-- local enableFundamentalAPI = ReactFeatureFlags.enableFundamentalAPI
local enableCreateEventHandleAPI = ReactFeatureFlags.enableCreateEventHandleAPI
-- local enableScopeAPI = ReactFeatureFlags.enableScopeAPI
-- local enableEagerRootListeners = ReactFeatureFlags.enableEagerRootListeners

-- local {HostComponent, HostText} = require(Packages.react-reconciler/src/ReactWorkTags')
-- local {
--   listenToReactEvent,
--   listenToAllSupportedEvents,
-- } = require(Packages.../events/DOMPluginEventSystem')

type Array<T> = { [number]: T }
type Object = { [any]: any }

-- ROBLOX deviation: Moved to ReactRobloxHostTypes
-- export type Type = string;
-- export type Props = {
--   autoFocus: boolean?,
--   children: any,
--   disabled: boolean?,
--   hidden: boolean?,
--   suppressHydrationWarning: boolean?,
--   dangerouslySetInnerHTML: any,
--   style: { display: string, [any]: any }?,
--   bottom: number?,
--   left: number?,
--   right: number?,
--   top: number?,
--   -- ...
--   [any]: any,
-- };
-- export type EventTargetChildElement = {
--   type: string,
--   props: nil | {
--     style?: {
--       position?: string,
--       zIndex?: number,
--       bottom?: string,
--       left?: string,
--       right?: string,
--       top?: string,
--       ...
--     },
--     ...
--   },
--   ...
-- end

-- ROBLOX deviation: Moved to ReactRobloxHostTypes
-- export type SuspenseInstance = Comment & {_reactRetry?: () => void, ...}
-- export type HydratableInstance = Instance | TextInstance | SuspenseInstance

-- ROBLOX deviation: Moved to ReactRobloxHostTypes
-- export type PublicInstance = Element | Text
-- type HostContextDev = {
--   namespace: string,
--   ancestorInfo: any,
--   -- ...
--   [any]: any,
-- }
-- type HostContextProd = string
-- export type HostContext = HostContextDev | HostContextProd

-- export type UpdatePayload = Array<mixed>
-- ROBLOX FIXME: cannot create type equal to void
-- export type ChildSet = void; -- Unused
-- export type TimeoutHandle = TimeoutID
-- export type NoTimeout = -1
-- export type RendererInspectionConfig = $ReadOnly<{or}>

-- export opaque type OpaqueIDType =
--   | string
--   | {
--       toString: () => string | void,
--       valueOf: () => string | void,
--     end

-- type SelectionInformation = {|
--   focusedElem: nil | HTMLElement,
--   selectionRange: mixed,
-- |}

-- local SUPPRESS_HYDRATION_WARNING
-- if __DEV__)
--   SUPPRESS_HYDRATION_WARNING = 'suppressHydrationWarning'
-- end

-- local SUSPENSE_START_DATA = '$'
-- local SUSPENSE_END_DATA = '/$'
-- local SUSPENSE_PENDING_START_DATA = '$?'
-- local SUSPENSE_FALLBACK_START_DATA = '$!'

-- local STYLE = 'style'

-- local eventsEnabled: boolean? = nil
-- local selectionInformation: nil | SelectionInformation = nil

-- function shouldAutoFocusHostComponent(type: string, props: Props): boolean {
--   switch (type)
--     case 'button':
--     case 'input':
--     case 'select':
--     case 'textarea':
--       return !!props.autoFocus
--   end
--   return false
-- end

-- ROBLOX deviation: Use GetDescendants rather than recursion
local function recursivelyUncacheFiberNode(node: HostInstance)
	-- ROBLOX https://jira.rbx.com/browse/LUAFDN-713: Tables are somehow ending up
	-- in this function that expects Instances. In that case, we won't be able to
	-- iterate through its descendants.
	if typeof(node :: any) ~= "Instance" then
		return
	end

	uncacheFiberNode(node)

	for _, child in node:GetDescendants() do
		uncacheFiberNode(child)
	end
end

local exports: { [any]: any } = {}
Object.assign(
	exports,
	require("@pkg/@jsdotlua/shared").ReactFiberHostConfig.WithNoPersistence
)

exports.getRootHostContext = function(rootContainerInstance: Container): HostContext
	-- ROBLOX deviation: This is a lot of HTML-DOM specific logic; I'm not clear on
	-- whether there'll be an equivalent of `namespaceURI` for our use cases, but
	-- we may want to provide other kinds of context for host objects.

	-- For now, as a guess, we'll return the kind of instance we're attached to
	return rootContainerInstance.ClassName

	-- local type
	-- local namespace
	-- local nodeType = rootContainerInstance.nodeType
	-- switch (nodeType)
	--   case DOCUMENT_NODE:
	--   case DOCUMENT_FRAGMENT_NODE: {
	--     type = nodeType == DOCUMENT_NODE ? '#document' : '#fragment'
	--     local root = (rootContainerInstance: any).documentElement
	--     namespace = root ? root.namespaceURI : getChildNamespace(null, '')
	--     break
	--   end
	--   default: {
	--     local container: any =
	--       nodeType == COMMENT_NODE
	--         ? rootContainerInstance.parentNode
	--         : rootContainerInstance
	--     local ownNamespace = container.namespaceURI or nil
	--     type = container.tagName
	--     namespace = getChildNamespace(ownNamespace, type)
	--     break
	--   end
	-- end
	-- if _G.__DEV__ then
	--   local validatedTag = type.toLowerCase()
	--   local ancestorInfo = updatedAncestorInfo(null, validatedTag)
	--   return {namespace, ancestorInfo}
	-- end
	-- return namespace
end

exports.getChildHostContext = function(
	parentHostContext: HostContext,
	type: string,
	rootContainerInstance: Container
): HostContext
	-- ROBLOX deviation: unclear on the purpose here just yet, might be fine to
	-- just return parent's hostContext for now
	return parentHostContext
	-- if _G.__DEV__ then
	--   local parentHostContextDev = ((parentHostContext: any): HostContextDev)
	--   local namespace = getChildNamespace(parentHostContextDev.namespace, type)
	--   local ancestorInfo = updatedAncestorInfo(
	--     parentHostContextDev.ancestorInfo,
	--     type,
	--   )
	--   return {namespace, ancestorInfo}
	-- end
	-- local parentNamespace = ((parentHostContext: any): HostContextProd)
	-- return getChildNamespace(parentNamespace, type)
end

exports.getPublicInstance = function(instance: Instance): any
	return instance
end

exports.prepareForCommit = function(containerInfo: Container): Object?
	-- eventsEnabled = ReactBrowserEventEmitterIsEnabled()
	-- selectionInformation = getSelectionInformation()
	local activeInstance = nil
	if enableCreateEventHandleAPI then
		unimplemented("enableCreateEventHandleAPI")
		--   local focusedElem = selectionInformation.focusedElem
		--   if focusedElem ~= nil then
		--     activeInstance = getClosestInstanceFromNode(focusedElem)
		--   end
	end
	-- ReactBrowserEventEmitterSetEnabled(false)
	return activeInstance
end

exports.beforeActiveInstanceBlur = function()
	if enableCreateEventHandleAPI then
		unimplemented("enableCreateEventHandleAPI")
		-- ReactBrowserEventEmitterSetEnabled(true)
		-- dispatchBeforeDetachedBlur((selectionInformation: any).focusedElem)
		-- ReactBrowserEventEmitterSetEnabled(false)
	end
end

exports.afterActiveInstanceBlur = function()
	if enableCreateEventHandleAPI then
		unimplemented("enableCreateEventHandleAPI")
		-- ReactBrowserEventEmitterSetEnabled(true)
		-- dispatchAfterDetachedBlur((selectionInformation: any).focusedElem)
		-- ReactBrowserEventEmitterSetEnabled(false)
	end
end

exports.resetAfterCommit = function(containerInfo: Container)
	-- warn("Skip unimplemented: resetAfterCommit")
	-- restoreSelection(selectionInformation)
	-- ReactBrowserEventEmitterSetEnabled(eventsEnabled)
	-- eventsEnabled = nil
	-- selectionInformation = nil
end

exports.createInstance = function(
	type_: string,
	props: Props,
	rootContainerInstance: Container,
	hostContext: HostContext,
	internalInstanceHandle: Object
): HostInstance
	-- local hostKey = virtualNode.hostKey

	local domElement = Instance.new(type_)
	-- ROBLOX deviation: compatibility with old Roact where instances have their name
	-- set to the key value
	if internalInstanceHandle.key then
		domElement.Name = internalInstanceHandle.key
	else
		local currentHandle = internalInstanceHandle.return_
		while currentHandle do
			if currentHandle.key then
				domElement.Name = currentHandle.key
				break
			end
			currentHandle = currentHandle.return_
		end
	end

	precacheFiberNode(internalInstanceHandle, domElement)
	updateFiberProps(domElement, props)

	-- TODO: Support refs (does that actually happen here, or later?)
	-- applyRef(element.props[Ref], instance)

	-- Will have to be managed outside of createInstance
	-- if virtualNode.eventManager ~= nil then
	--   virtualNode.eventManager:resume()
	-- end

	return domElement

	-- return Instance.new("Frame")
	-- local parentNamespace: string
	-- if __DEV__)
	--   -- TODO: take namespace into account when validating.
	--   local hostContextDev = ((hostContext: any): HostContextDev)
	--   validateDOMNesting(type, nil, hostContextDev.ancestorInfo)
	--   if
	--     typeof props.children == 'string' or
	--     typeof props.children == 'number'
	--   )
	--     local string = '' + props.children
	--     local ownAncestorInfo = updatedAncestorInfo(
	--       hostContextDev.ancestorInfo,
	--       type,
	--     )
	--     validateDOMNesting(null, string, ownAncestorInfo)
	--   end
	--   parentNamespace = hostContextDev.namespace
	-- } else {
	--   parentNamespace = ((hostContext: any): HostContextProd)
	-- end
	-- local domElement: Instance = createElement(
	--   type,
	--   props,
	--   rootContainerInstance,
	--   parentNamespace,
	-- )
end

exports.appendInitialChild = function(parentInstance: Instance, child: Instance)
	-- ROBLOX deviation: Establish hierarchy with Parent property
	child.Parent = parentInstance
end

exports.finalizeInitialChildren = function(
	domElement: HostInstance,
	type_: string,
	props: Props,
	rootContainerInstance: Container,
	hostContext: HostContext
): boolean
	setInitialProperties(domElement, type_, props, rootContainerInstance)
	return false
	-- return shouldAutoFocusHostComponent(type_, props)
end

local function prepareUpdate(
	domElement: Instance,
	type_: string,
	oldProps: Props,
	newProps: Props,
	rootContainerInstance: Container,
	hostContext: HostContext
): nil | Array<any>
	-- if _G.__DEV__ then
	--   local hostContextDev = ((hostContext: any): HostContextDev)
	--   if
	--     typeof newProps.children ~= typeof oldProps.children and
	--     (typeof newProps.children == 'string' or
	--       typeof newProps.children == 'number')
	--   )
	--     local string = '' + newProps.children
	--     local ownAncestorInfo = updatedAncestorInfo(
	--       hostContextDev.ancestorInfo,
	--       type,
	--     )
	--     validateDOMNesting(null, string, ownAncestorInfo)
	--   end
	-- end
	return diffProperties(domElement, type_, oldProps, newProps, rootContainerInstance)
end
exports.prepareUpdate = prepareUpdate

exports.shouldSetTextContent = function(_type: string, _props: Props): boolean
	-- ROBLOX deviation: Ignore TextInstance logic, which isn't applicable to Roblox
	return false
	--   return (
	--     type == 'textarea' or
	--     type == 'option' or
	--     type == 'noscript' or
	--     typeof props.children == 'string' or
	--     typeof props.children == 'number' or
	--     (typeof props.dangerouslySetInnerHTML == 'table’' and
	--       props.dangerouslySetInnerHTML ~= nil and
	--       props.dangerouslySetInnerHTML.__html ~= nil)
	--   )
end

-- ROBLOX deviation: Text nodes aren't supported in Roblox renderer, so error so that tests fail immediately
exports.createTextInstance = function(
	text: string,
	rootContainerInstance: Container,
	hostContext: HostContext,
	internalInstanceHandle: Object
): any
	unimplemented("createTextInstance")
	return nil
end

exports.isPrimaryRenderer = true
exports.warnsIfNotActing = true
-- This initialization code may run even on server environments
-- if a component just imports ReactDOM (e.g. for findDOMNode).
-- Some environments might not have setTimeout or clearTimeout.
-- ROBLOX deviation: We're only dealing with client right now, so these always populate
exports.scheduleTimeout = setTimeout
exports.cancelTimeout = clearTimeout
exports.noTimeout = -1

-- -------------------
--     Mutation
-- -------------------

exports.supportsMutation = true

exports.commitMount = function(
	domElement: Instance,
	type: string,
	newProps: Props,
	internalInstanceHandle: Object
)
	unimplemented("commitMount")
	-- -- Despite the naming that might imply otherwise, this method only
	-- -- fires if there is an `Update` effect scheduled during mounting.
	-- -- This happens if `finalizeInitialChildren` returns `true` (which it
	-- -- does to implement the `autoFocus` attribute on the client). But
	-- -- there are also other cases when this might happen (such as patching
	-- -- up text content during hydration mismatch). So we'll check this again.
	-- if shouldAutoFocusHostComponent(type, newProps))
	--   ((domElement: any):
	--     | HTMLButtonElement
	--     | HTMLInputElement
	--     | HTMLSelectElement
	--     | HTMLTextAreaElement).focus()
	-- end
end

exports.commitUpdate = function(
	domElement: Instance,
	updatePayload: Array<any>,
	type_: string,
	oldProps: Props,
	newProps: Props,
	internalInstanceHandle: Object
)
	-- Update the props handle so that we know which props are the ones with
	-- with current event handlers.
	updateFiberProps(domElement, newProps)
	-- Apply the diff to the DOM node.
	updateProperties(domElement, updatePayload, oldProps)
end

-- ROBLOX deviation: Ignore TextInstance logic, which isn't applicable to Roblox
-- exports.resetTextContent(domElement: Instance): void {
--   setTextContent(domElement, '')
-- end

-- ROBLOX deviation: Ignore TextInstance logic, which isn't applicable to Roblox
-- exports.commitTextUpdate(
--   textInstance: TextInstance,
--   oldText: string,
--   newText: string,
-- ): void {
--   textInstance.nodeValue = newText
-- end

local function checkTags(instance: Instance)
	if typeof(instance :: any) ~= "Instance" then
		console.warn("Could not check tags on non-instance %s.", inspect(instance))
		return
	end
	if not instance:IsDescendantOf(game) then
		if #CollectionService:GetTags(instance) > 0 then
			console.warn(
				'Tags applied to orphaned %s "%s" cannot be accessed via'
					.. " CollectionService:GetTagged. If you're relying on tag"
					.. " behavior in a unit test, consider mounting your test "
					.. "root into the DataModel.",
				instance.ClassName,
				instance.Name
			)
		end
	end
end

exports.appendChild = function(parentInstance: Instance, child: Instance)
	-- ROBLOX deviation: Roblox's DOM is based on child->parent references
	child.Parent = parentInstance
	-- parentInstance.appendChild(child)
	if _G.__DEV__ then
		checkTags(child)
	end
end

exports.appendChildToContainer = function(container: Container, child: Instance)
	-- ROBLOX TODO: Some of this logic may come back; for now, keep it simple
	local parentNode = container
	exports.appendChild(parentNode, child)

	-- if container.nodeType == COMMENT_NODE)
	--   parentNode = (container.parentNode: any)
	--   parentNode.insertBefore(child, container)
	-- } else {
	--   parentNode = container
	--   parentNode.appendChild(child)
	-- end
	-- -- This container might be used for a portal.
	-- -- If something inside a portal is clicked, that click should bubble
	-- -- through the React tree. However, on Mobile Safari the click would
	-- -- never bubble through the *DOM* tree unless an ancestor with onclick
	-- -- event exists. So we wouldn't see it and dispatch it.
	-- -- This is why we ensure that non React root containers have inline onclick
	-- -- defined.
	-- -- https://github.com/facebook/react/issues/11918
	-- local reactRootContainer = container._reactRootContainer
	-- if
	--   reactRootContainer == nil and parentNode.onclick == nil
	-- then
	--   -- TODO: This cast may not be sound for SVG, MathML or custom elements.
	--   trapClickOnNonInteractiveElement(((parentNode: any): HTMLElement))
	-- end
end

exports.insertBefore =
	function(parentInstance: Instance, child: Instance, _beforeChild: Instance)
		-- ROBLOX deviation: Roblox's DOM is based on child->parent references
		child.Parent = parentInstance
		-- parentInstance.insertBefore(child, beforeChild)
		if _G.__DEV__ then
			checkTags(child)
		end
	end

exports.insertInContainerBefore =
	function(container: Container, child: Instance, beforeChild: Instance)
		-- ROBLOX deviation: use our container definition
		local parentNode = container
		exports.insertBefore(parentNode, child, beforeChild)
		-- if container.nodeType == COMMENT_NODE)
		--   (container.parentNode: any).insertBefore(child, beforeChild)
		-- } else {
		--   container.insertBefore(child, beforeChild)
		-- end
	end

-- function createEvent(type: DOMEventName, bubbles: boolean): Event {
--   local event = document.createEvent('Event')
--   event.initEvent(((type: any): string), bubbles, false)
--   return event
-- end

-- function dispatchBeforeDetachedBlur(target: HTMLElement): void {
--   if enableCreateEventHandleAPI)
--     local event = createEvent('beforeblur', true)
--     -- Dispatch "beforeblur" directly on the target,
--     -- so it gets picked up by the event system and
--     -- can propagate through the React internal tree.
--     target.dispatchEvent(event)
--   end
-- end

-- function dispatchAfterDetachedBlur(target: HTMLElement): void {
--   if enableCreateEventHandleAPI)
--     local event = createEvent('afterblur', false)
--     -- So we know what was detached, make the relatedTarget the
--     -- detached target on the "afterblur" event.
--     (event: any).relatedTarget = target
--     -- Dispatch the event on the document.
--     document.dispatchEvent(event)
--   end
-- end

exports.removeChild = function(_parentInstance: Instance, child: Instance)
	recursivelyUncacheFiberNode(child)
	-- ROBLOX deviation: The roblox renderer tracks bindings and event managers
	-- for instances, so make sure we clean those up when we remove the instance
	cleanupHostComponent(child)
	-- ROBLOX deviation: Roblox's DOM is based on child->parent references
	child.Parent = nil
	-- parentInstance.removeChild(child)
	-- ROBLOX deviation: Guard against misuse by locking parent and forcing external cleanup via Destroy
	child:Destroy()
end

exports.removeChildFromContainer = function(_container: Container, child: Instance)
	-- ROBLOX deviation: Containers don't have special behavior and comment nodes
	-- have no datamodel equivalent, so just forward to the removeChild logic
	exports.removeChild(_container, child)
	-- if container.nodeType == COMMENT_NODE)
	--   (container.parentNode: any).removeChild(child)
	-- } else {
	--   container.removeChild(child)
	-- end
end

exports.clearSuspenseBoundary =
	function(parentInstance: Instance, suspenseInstance: SuspenseInstance)
		-- ROBLOX FIXME: this is a major thing we need to fix for Suspense to work as a feature
		unimplemented("clearSuspenseBoundary")
		--   local node = suspenseInstance
		--   -- Delete all nodes within this suspense boundary.
		--   -- There might be nested nodes so we need to keep track of how
		--   -- deep we are and only break out when we're back on top.
		--   local depth = 0
		--   do {
		--     local nextNode = node.nextSibling
		--     parentInstance.removeChild(node)
		--     if nextNode and nextNode.nodeType == COMMENT_NODE)
		--       local data = ((nextNode: any).data: string)
		--       if data == SUSPENSE_END_DATA)
		--         if depth == 0)
		--           parentInstance.removeChild(nextNode)
		--           -- Retry if any event replaying was blocked on this.
		--           retryIfBlockedOn(suspenseInstance)
		--           return
		--         } else {
		--           depth--
		--         end
		--       } else if
		--         data == SUSPENSE_START_DATA or
		--         data == SUSPENSE_PENDING_START_DATA or
		--         data == SUSPENSE_FALLBACK_START_DATA
		--       )
		--         depth++
		--       end
		--     end
		--     node = nextNode
		--   } while (node)
		--   -- TODO: Warn, we didn't find the end comment boundary.
		--   -- Retry if any event replaying was blocked on this.
		--   retryIfBlockedOn(suspenseInstance)
	end

exports.clearSuspenseBoundaryFromContainer =
	function(container: Container, suspenseInstance: SuspenseInstance)
		-- ROBLOX FIXME: this is a major thing we need to fix for Suspense to work as a feature
		unimplemented("clearSuspenseBoundaryFromContainer")
		--   if container.nodeType == COMMENT_NODE)
		--     clearSuspenseBoundary((container.parentNode: any), suspenseInstance)
		--   } else if container.nodeType == ELEMENT_NODE)
		--     clearSuspenseBoundary((container: any), suspenseInstance)
		--   } else {
		--     -- Document nodes should never contain suspense boundaries.
		--   end
		--   -- Retry if any event replaying was blocked on this.
		--   retryIfBlockedOn(container)
	end

exports.hideInstance = function(instance: Instance)
	unimplemented("hideInstance")
	-- -- TODO: Does this work for all element types? What about MathML? Should we
	-- -- pass host context to this method?
	-- instance = ((instance: any): HTMLElement)
	-- local style = instance.style
	-- if typeof style.setProperty == 'function')
	--   style.setProperty('display', 'none', 'important')
	-- } else {
	--   style.display = 'none'
	-- end
end

-- ROBLOX deviation: error on TextInstance logic, which isn't applicable to Roblox
exports.hideTextInstance = function(textInstance: TextInstance): ()
	unimplemented("hideTextInstance")
	--   textInstance.nodeValue = ''
end

exports.unhideInstance = function(instance: Instance, props: Props)
	unimplemented("unhideInstance")
	-- instance = ((instance: any): HTMLElement)
	-- local styleProp = props[STYLE]
	-- local display =
	--   styleProp ~= undefined and
	--   styleProp ~= nil and
	--   styleProp.hasOwnProperty('display')
	--     ? styleProp.display
	--     : nil
	-- instance.style.display = dangerousStyleValue('display', display)
end

-- ROBLOX deviation: error on TextInstance logic, which isn't applicable to Roblox
exports.unhideTextInstance = function(textInstance: TextInstance, text: string): ()
	unimplemented("unhideTextInstance")
	--   textInstance.nodeValue = text
end

exports.clearContainer = function(container: Container)
	-- ROBLOX deviation: with Roblox, we can simply enumerate and remove the children
	local parentInstance = container
	for _, child in parentInstance:GetChildren() do
		exports.removeChild(parentInstance, child)
	end
	-- if container.nodeType == ELEMENT_NODE)
	--   ((container: any): Element).textContent = ''
	-- } else if container.nodeType == DOCUMENT_NODE)
	--   local body = ((container: any): Document).body
	--   if body ~= nil)
	--     body.textContent = ''
	--   end
	-- end
end

-- -- -------------------
-- --     Hydration
-- -- -------------------

-- export local supportsHydration = true

-- exports.canHydrateInstance(
--   instance: HydratableInstance,
--   type: string,
--   props: Props,
-- ): nil | Instance {
--   if
--     instance.nodeType ~= ELEMENT_NODE or
--     type.toLowerCase() ~= instance.nodeName.toLowerCase()
--   )
--     return nil
--   end
--   -- This has now been refined to an element node.
--   return ((instance: any): Instance)
-- end

-- exports.canHydrateTextInstance(
--   instance: HydratableInstance,
--   text: string,
-- ): nil | TextInstance {
--   if text == '' or instance.nodeType ~= TEXT_NODE)
--     -- Empty strings are not parsed by HTML so there won't be a correct match here.
--     return nil
--   end
--   -- This has now been refined to a text node.
--   return ((instance: any): TextInstance)
-- end

-- exports.canHydrateSuspenseInstance(
--   instance: HydratableInstance,
-- ): nil | SuspenseInstance {
--   if instance.nodeType ~= COMMENT_NODE)
--     -- Empty strings are not parsed by HTML so there won't be a correct match here.
--     return nil
--   end
--   -- This has now been refined to a suspense node.
--   return ((instance: any): SuspenseInstance)
-- end

-- exports.isSuspenseInstanceFallback(instance: SuspenseInstance)
--   return instance.data == SUSPENSE_FALLBACK_START_DATA
-- end

-- exports.registerSuspenseInstanceRetry(
--   instance: SuspenseInstance,
--   callback: () => void,
-- )
--   instance._reactRetry = callback
-- end

-- function getNextHydratable(node)
--   -- Skip non-hydratable nodes.
--   for (; node ~= nil; node = node.nextSibling)
--     local nodeType = node.nodeType
--     if nodeType == ELEMENT_NODE or nodeType == TEXT_NODE)
--       break
--     end
--     if enableSuspenseServerRenderer)
--       if nodeType == COMMENT_NODE)
--         local nodeData = (node: any).data
--         if
--           nodeData == SUSPENSE_START_DATA or
--           nodeData == SUSPENSE_FALLBACK_START_DATA or
--           nodeData == SUSPENSE_PENDING_START_DATA
--         )
--           break
--         end
--       end
--     end
--   end
--   return (node: any)
-- end

-- exports.getNextHydratableSibling(
--   instance: HydratableInstance,
-- ): nil | HydratableInstance {
--   return getNextHydratable(instance.nextSibling)
-- end

-- exports.getFirstHydratableChild(
--   parentInstance: Container | Instance,
-- ): nil | HydratableInstance {
--   return getNextHydratable(parentInstance.firstChild)
-- end

-- exports.hydrateInstance(
--   instance: Instance,
--   type: string,
--   props: Props,
--   rootContainerInstance: Container,
--   hostContext: HostContext,
--   internalInstanceHandle: Object,
-- ): nil | Array<mixed> {
--   precacheFiberNode(internalInstanceHandle, instance)
--   -- TODO: Possibly defer this until the commit phase where all the events
--   -- get attached.
--   updateFiberProps(instance, props)
--   local parentNamespace: string
--   if __DEV__)
--     local hostContextDev = ((hostContext: any): HostContextDev)
--     parentNamespace = hostContextDev.namespace
--   } else {
--     parentNamespace = ((hostContext: any): HostContextProd)
--   end
--   return diffHydratedProperties(
--     instance,
--     type,
--     props,
--     parentNamespace,
--     rootContainerInstance,
--   )
-- end

-- exports.hydrateTextInstance(
--   textInstance: TextInstance,
--   text: string,
--   internalInstanceHandle: Object,
-- ): boolean {
--   precacheFiberNode(internalInstanceHandle, textInstance)
--   return diffHydratedText(textInstance, text)
-- end

-- exports.hydrateSuspenseInstance(
--   suspenseInstance: SuspenseInstance,
--   internalInstanceHandle: Object,
-- )
--   precacheFiberNode(internalInstanceHandle, suspenseInstance)
-- end

-- exports.getNextHydratableInstanceAfterSuspenseInstance(
--   suspenseInstance: SuspenseInstance,
-- ): nil | HydratableInstance {
--   local node = suspenseInstance.nextSibling
--   -- Skip past all nodes within this suspense boundary.
--   -- There might be nested nodes so we need to keep track of how
--   -- deep we are and only break out when we're back on top.
--   local depth = 0
--   while (node)
--     if node.nodeType == COMMENT_NODE)
--       local data = ((node: any).data: string)
--       if data == SUSPENSE_END_DATA)
--         if depth == 0)
--           return getNextHydratableSibling((node: any))
--         } else {
--           depth--
--         end
--       } else if
--         data == SUSPENSE_START_DATA or
--         data == SUSPENSE_FALLBACK_START_DATA or
--         data == SUSPENSE_PENDING_START_DATA
--       )
--         depth++
--       end
--     end
--     node = node.nextSibling
--   end
--   -- TODO: Warn, we didn't find the end comment boundary.
--   return nil
-- end

-- -- Returns the SuspenseInstance if this node is a direct child of a
-- -- SuspenseInstance. I.e. if its previous sibling is a Comment with
-- -- SUSPENSE_x_START_DATA. Otherwise, nil.
-- exports.getParentSuspenseInstance(
--   targetInstance: Node,
-- ): nil | SuspenseInstance {
--   local node = targetInstance.previousSibling
--   -- Skip past all nodes within this suspense boundary.
--   -- There might be nested nodes so we need to keep track of how
--   -- deep we are and only break out when we're back on top.
--   local depth = 0
--   while (node)
--     if node.nodeType == COMMENT_NODE)
--       local data = ((node: any).data: string)
--       if
--         data == SUSPENSE_START_DATA or
--         data == SUSPENSE_FALLBACK_START_DATA or
--         data == SUSPENSE_PENDING_START_DATA
--       )
--         if depth == 0)
--           return ((node: any): SuspenseInstance)
--         } else {
--           depth--
--         end
--       } else if data == SUSPENSE_END_DATA)
--         depth++
--       end
--     end
--     node = node.previousSibling
--   end
--   return nil
-- end

-- exports.commitHydratedContainer(container: Container): void {
--   -- Retry if any event replaying was blocked on this.
--   retryIfBlockedOn(container)
-- end

-- exports.commitHydratedSuspenseInstance(
--   suspenseInstance: SuspenseInstance,
-- ): void {
--   -- Retry if any event replaying was blocked on this.
--   retryIfBlockedOn(suspenseInstance)
-- end

-- exports.didNotMatchHydratedContainerTextInstance(
--   parentContainer: Container,
--   textInstance: TextInstance,
--   text: string,
-- )
--   if __DEV__)
--     warnForUnmatchedText(textInstance, text)
--   end
-- end

-- exports.didNotMatchHydratedTextInstance(
--   parentType: string,
--   parentProps: Props,
--   parentInstance: Instance,
--   textInstance: TextInstance,
--   text: string,
-- )
--   if __DEV__ and parentProps[SUPPRESS_HYDRATION_WARNING] ~= true)
--     warnForUnmatchedText(textInstance, text)
--   end
-- end

-- exports.didNotHydrateContainerInstance(
--   parentContainer: Container,
--   instance: HydratableInstance,
-- )
--   if __DEV__)
--     if instance.nodeType == ELEMENT_NODE)
--       warnForDeletedHydratableElement(parentContainer, (instance: any))
--     } else if instance.nodeType == COMMENT_NODE)
--       -- TODO: warnForDeletedHydratableSuspenseBoundary
--     } else {
--       warnForDeletedHydratableText(parentContainer, (instance: any))
--     end
--   end
-- end

-- exports.didNotHydrateInstance(
--   parentType: string,
--   parentProps: Props,
--   parentInstance: Instance,
--   instance: HydratableInstance,
-- )
--   if __DEV__ and parentProps[SUPPRESS_HYDRATION_WARNING] ~= true)
--     if instance.nodeType == ELEMENT_NODE)
--       warnForDeletedHydratableElement(parentInstance, (instance: any))
--     } else if instance.nodeType == COMMENT_NODE)
--       -- TODO: warnForDeletedHydratableSuspenseBoundary
--     } else {
--       warnForDeletedHydratableText(parentInstance, (instance: any))
--     end
--   end
-- end

-- exports.didNotFindHydratableContainerInstance(
--   parentContainer: Container,
--   type: string,
--   props: Props,
-- )
--   if __DEV__)
--     warnForInsertedHydratedElement(parentContainer, type, props)
--   end
-- end

-- exports.didNotFindHydratableContainerTextInstance(
--   parentContainer: Container,
--   text: string,
-- )
--   if __DEV__)
--     warnForInsertedHydratedText(parentContainer, text)
--   end
-- end

-- exports.didNotFindHydratableContainerSuspenseInstance(
--   parentContainer: Container,
-- )
--   if __DEV__)
--     -- TODO: warnForInsertedHydratedSuspense(parentContainer)
--   end
-- end

-- exports.didNotFindHydratableInstance(
--   parentType: string,
--   parentProps: Props,
--   parentInstance: Instance,
--   type: string,
--   props: Props,
-- )
--   if __DEV__ and parentProps[SUPPRESS_HYDRATION_WARNING] ~= true)
--     warnForInsertedHydratedElement(parentInstance, type, props)
--   end
-- end

-- exports.didNotFindHydratableTextInstance(
--   parentType: string,
--   parentProps: Props,
--   parentInstance: Instance,
--   text: string,
-- )
--   if __DEV__ and parentProps[SUPPRESS_HYDRATION_WARNING] ~= true)
--     warnForInsertedHydratedText(parentInstance, text)
--   end
-- end

-- exports.didNotFindHydratableSuspenseInstance(
--   parentType: string,
--   parentProps: Props,
--   parentInstance: Instance,
-- )
--   if __DEV__ and parentProps[SUPPRESS_HYDRATION_WARNING] ~= true)
--     -- TODO: warnForInsertedHydratedSuspense(parentInstance)
--   end
-- end

-- exports.getFundamentalComponentInstance(
--   fundamentalInstance: ReactDOMFundamentalComponentInstance,
-- ): Instance {
--   if enableFundamentalAPI)
--     local {currentFiber, impl, props, state} = fundamentalInstance
--     local instance = impl.getInstance(null, props, state)
--     precacheFiberNode(currentFiber, instance)
--     return instance
--   end
--   -- Because of the flag above, this gets around the Flow error
--   return (null: any)
-- end

-- exports.mountFundamentalComponent(
--   fundamentalInstance: ReactDOMFundamentalComponentInstance,
-- ): void {
--   if enableFundamentalAPI)
--     local {impl, instance, props, state} = fundamentalInstance
--     local onMount = impl.onMount
--     if onMount ~= undefined)
--       onMount(null, instance, props, state)
--     end
--   end
-- end

-- exports.shouldUpdateFundamentalComponent(
--   fundamentalInstance: ReactDOMFundamentalComponentInstance,
-- ): boolean {
--   if enableFundamentalAPI)
--     local {impl, prevProps, props, state} = fundamentalInstance
--     local shouldUpdate = impl.shouldUpdate
--     if shouldUpdate ~= undefined)
--       return shouldUpdate(null, prevProps, props, state)
--     end
--   end
--   return true
-- end

-- exports.updateFundamentalComponent(
--   fundamentalInstance: ReactDOMFundamentalComponentInstance,
-- ): void {
--   if enableFundamentalAPI)
--     local {impl, instance, prevProps, props, state} = fundamentalInstance
--     local onUpdate = impl.onUpdate
--     if onUpdate ~= undefined)
--       onUpdate(null, instance, prevProps, props, state)
--     end
--   end
-- end

-- exports.unmountFundamentalComponent(
--   fundamentalInstance: ReactDOMFundamentalComponentInstance,
-- ): void {
--   if enableFundamentalAPI)
--     local {impl, instance, props, state} = fundamentalInstance
--     local onUnmount = impl.onUnmount
--     if onUnmount ~= undefined)
--       onUnmount(null, instance, props, state)
--     end
--   end
-- end

-- exports.getInstanceFromNode(node: HTMLElement): nil | Object {
--   return getClosestInstanceFromNode(node) or nil
-- end

-- local clientId: number = 0
-- exports.makeClientId(): OpaqueIDType {
--   return 'r:' + (clientId++).toString(36)
-- end

-- exports.makeClientIdInDEV(warnOnAccessInDEV: () => void): OpaqueIDType {
--   local id = 'r:' + (clientId++).toString(36)
--   return {
--     toString()
--       warnOnAccessInDEV()
--       return id
--     },
--     valueOf()
--       warnOnAccessInDEV()
--       return id
--     },
--   end
-- end

-- exports.isOpaqueHydratingObject(value: mixed): boolean {
--   return (
--     value ~= nil and
--     typeof value == 'table’' and
--     value.$$typeof == REACT_OPAQUE_ID_TYPE
--   )
-- end

-- exports.makeOpaqueHydratingObject(
--   attemptToReadValue: () => void,
-- ): OpaqueIDType {
--   return {
--     $$typeof: REACT_OPAQUE_ID_TYPE,
--     toString: attemptToReadValue,
--     valueOf: attemptToReadValue,
--   end
-- end

exports.preparePortalMount = function(portalInstance: Instance): ()
	-- ROBLOX TODO: Revisit this logic and see if any of it applies
	-- if enableEagerRootListeners then
	--   listenToAllSupportedEvents(portalInstance)
	-- else
	--   listenToReactEvent('onMouseEnter', portalInstance)
	-- end
end

-- exports.prepareScopeUpdate(
--   scopeInstance: ReactScopeInstance,
--   internalInstanceHandle: Object,
-- ): void {
--   if enableScopeAPI)
--     precacheFiberNode(internalInstanceHandle, scopeInstance)
--   end
-- end

-- exports.getInstanceFromScope(
--   scopeInstance: ReactScopeInstance,
-- ): nil | Object {
--   if enableScopeAPI)
--     return getFiberFromScopeInstance(scopeInstance)
--   end
--   return nil
-- end

-- export local supportsTestSelectors = true

-- exports.findFiberRoot(node: Instance): nil | FiberRoot {
--   local stack = [node]
--   local index = 0
--   while (index < stack.length)
--     local current = stack[index++]
--     if isContainerMarkedAsRoot(current))
--       return ((getInstanceFromNodeDOMTree(current): any): FiberRoot)
--     end
--     stack.push(...current.children)
--   end
--   return nil
-- end

-- exports.getBoundingRect(node: Instance): BoundingRect {
--   local rect = node.getBoundingClientRect()
--   return {
--     x: rect.left,
--     y: rect.top,
--     width: rect.width,
--     height: rect.height,
--   end
-- end

-- exports.matchAccessibilityRole(node: Instance, role: string): boolean {
--   if hasRole(node, role))
--     return true
--   end

--   return false
-- end

-- exports.getTextContent(fiber: Fiber): string | nil {
--   switch (fiber.tag)
--     case HostComponent:
--       local textContent = ''
--       local childNodes = fiber.stateNode.childNodes
--       for (local i = 0; i < childNodes.length; i++)
--         local childNode = childNodes[i]
--         if childNode.nodeType == Node.TEXT_NODE)
--           textContent += childNode.textContent
--         end
--       end
--       return textContent
--     case HostText:
--       return fiber.stateNode.textContent
--   end

--   return nil
-- end

-- exports.isHiddenSubtree(fiber: Fiber): boolean {
--   return fiber.tag == HostComponent and fiber.memoizedProps.hidden == true
-- end

-- exports.setFocusIfFocusable(node: Instance): boolean {
--   -- The logic for determining if an element is focusable is kind of complex,
--   -- and since we want to actually change focus anyway- we can just skip it.
--   -- Instead we'll just listen for a "focus" event to verify that focus was set.
--   --
--   -- We could compare the node to document.activeElement after focus,
--   -- but this would not handle the case where application code managed focus to automatically blur.
--   local didFocus = false
--   local handleFocus = () => {
--     didFocus = true
--   end

--   local element = ((node: any): HTMLElement)
--   try {
--     element.addEventListener('focus', handleFocus)
--     (element.focus or HTMLElement.prototype.focus).call(element)
--   } finally {
--     element.removeEventListener('focus', handleFocus)
--   end

--   return didFocus
-- end

-- type RectRatio = {
--   ratio: number,
--   rect: BoundingRect,
-- end

-- exports.setupIntersectionObserver(
--   targets: Array<Instance>,
--   callback: ObserveVisibleRectsCallback,
--   options?: IntersectionObserverOptions,
-- ): {|
--   disconnect: () => void,
--   observe: (instance: Instance) => void,
--   unobserve: (instance: Instance) => void,
-- |} {
--   local rectRatioCache: Map<Instance, RectRatio> = new Map()
--   targets.forEach(target => {
--     rectRatioCache.set(target, {
--       rect: getBoundingRect(target),
--       ratio: 0,
--     })
--   })

--   local handleIntersection = (entries: Array<IntersectionObserverEntry>) => {
--     entries.forEach(entry => {
--       local {boundingClientRect, intersectionRatio, target} = entry
--       rectRatioCache.set(target, {
--         rect: {
--           x: boundingClientRect.left,
--           y: boundingClientRect.top,
--           width: boundingClientRect.width,
--           height: boundingClientRect.height,
--         },
--         ratio: intersectionRatio,
--       })
--     })

--     callback(Array.from(rectRatioCache.values()))
--   end

--   local observer = new IntersectionObserver(handleIntersection, options)
--   targets.forEach(target => {
--     observer.observe((target: any))
--   })

--   return {
--     disconnect: () => observer.disconnect(),
--     observe: target => {
--       rectRatioCache.set(target, {
--         rect: getBoundingRect(target),
--         ratio: 0,
--       })
--       observer.observe((target: any))
--     },
--     unobserve: target => {
--       rectRatioCache.delete(target)
--       observer.unobserve((target: any))
--     },
--   end
-- end

return exports
