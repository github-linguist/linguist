-- emitter.tl
--
-- Copyright (c) 2024, Michael Dowling
--
-- This module is free software; you can redistribute it and/or modify it under
-- the terms of the MIT license. See LICENSE for details.

--- Signal slot event dispatcher.
local record Emitter
    --- An event that can be emitted.
    ---
    --- Events are registered with an emitter by their type, and the type is defined using a sentinel value of the
    --- same type as the event. This is typically done using a record for the event, and each instance of the event
    --- sets its type to the record.
    interface Event
        --- The type of event used to lookup the event by type.
        type: self
    end

    --- Receives a specific type of emitted events.
    type Listener = function<E is Event>(event: E)

    --- Optional configuration used to configure a listener.
    record ListenerConfig
        id: string
        position: ListenerPosition
    end

    --- The position to insert a listener.
    enum ListenerPosition
        --- Insert the listener at the beginning of the list of listeners.
        "first"
        --- Insert the listener at the end of the list of listeners.
        "last"
    end

    --- Create a new Emitter.
    --- @return the created Emitter
    new: function(): Emitter

    --- Add an event listener to the object.
    --- @param event    The event type to subscribe to.
    --- @param listener Listener to invoke.
    --- @param config?  Optional listener configuration.
    on: function<E is Event>(self: Emitter, event: E, listener: Listener<E>, config?: ListenerConfig)

    --- Unsubscribes a listener from an event.
    --- @param event    Event to unsubscribe from.
    --- @param listener Listener function or ID to unsubscribe.
    off: function<E is Event>(self: Emitter, event: E, listener: Listener<E> | string)

    --- Add an event listener that is unsubscribed after receiving an event.
    --- @param event    The event type to subscribe to.
    --- @param listener Listener to invoke.
    --- @param config?  Optional listener configuration.
    once: function<E is Event>(self: Emitter, event: E, listener: Listener<E>, config?: ListenerConfig)

    --- Emit an event to all listners.
    --- @param event Event to emit.
    emit: function(self: Emitter, event: Event)

    --- Forwards all events of this Emitter to the provided `emitter`.
    --- @param emitter Emitter that receives all events of the this emitter.
    startForwarding: function(self: Emitter, emitter: Emitter)

    --- Stops forwarding events of this Emitter to the provided `emitter`.
    --- @param emitter the emitter to remove.
    stopForwarding: function(self: Emitter, emitter: Emitter)

    --- Resets the state of the emitter by removing all listeners and stops all forwarding.
    reset: function(self: Emitter)

    --- Unsubscribes all listeners for a specific event type.
    --- @param name Event to unsubscribe.
    removeAllListeners: function(self: Emitter, event: Event)

    _forwarding: LinkedList<EmitterNode>
    _listeners: {any:LinkedList<ListenerNode>}
end

-----------------------------------------------------------------------------

--- A doubly linked list is used for event listeners and forwarding emitters so they can be removed during traversal.
local interface ListNode<N is ListNode>
    next: N
    prev: N
end

local record LinkedList<N is ListNode<any>>
    head: N
    tail: N
end

function LinkedList.append<N is ListNode<any>, L is LinkedList<N>>(list: L, node: N)
    if list.head == nil then
        list.head = node
        list.tail = node
    else
        list.tail.next = node
        node.prev = list.tail
        list.tail = node
    end
end

function LinkedList.prepend<N is ListNode<any>, L is LinkedList<N>>(list: L, node: N)
    if list.head == nil then
        list.head = node
        list.tail = node
    else
        list.head.prev = node
        node.next = list.head
        list.head = node
    end
end

function LinkedList.remove<N is ListNode<any>, L is LinkedList<N>>(list: L, node: N)
    if node.prev then
        (node.prev as N).next = node.next
    else
        list.head = node.next as ListNode<any>
    end
    if node.next then
        (node.next as N).prev = node.prev
    else
        list.tail = node.prev as ListNode<any>
    end
end

local record ListenerNode is ListNode<ListenerNode>
    id: string
    listener: Emitter.Listener<any>
end

local record EmitterNode is ListNode<EmitterNode>
    emitter: Emitter
end

-----------------------------------------------------------------------------

local DEFAULT_CONFIG <const>: Emitter.ListenerConfig = { id = "", position = "last" }
local EMITTER_MT <const>: metatable<Emitter> = { __index = Emitter }

function Emitter.new(): Emitter
    return setmetatable({
        _listeners = {},
        _forwarding = {}
    }, EMITTER_MT)
end

function Emitter:reset()
    self._listeners = {}
    self._forwarding = {}
end

function Emitter:on<E is Emitter.Event>(event: E, listener: Emitter.Listener<E>, config?: Emitter.ListenerConfig)
    local list = self._listeners[event]
    if not list then
        list = {}
        self._listeners[event] = list
    end
    config = config or DEFAULT_CONFIG
    local node <const>: ListenerNode = { id = config.id or "", listener = listener as Emitter.Listener<any> }
    if config.position == "last" then
        LinkedList.append(list, node as ListNode<any>)
    else
        LinkedList.prepend(list, node as ListNode<any>)
    end
end

function Emitter:off<E is Emitter.Event>(event: E, listener: Emitter.Listener<E> | string)
    local listeners <const> = self._listeners[event]
    if not listeners then
        return
    elseif listener is string then
        local node = listeners.head
        while node do
            if node.id == listener then
                LinkedList.remove(listeners, node as ListNode<any>)
            end
            node = node.next
        end
    else
        local node = listeners.head
        while node do
            if node.listener == listener as Emitter.Listener<any> then
                LinkedList.remove(listeners, node as ListNode<any>)
            end
            node = node.next
        end
    end
end

function Emitter:once<E is Emitter.Event>(event: E, listener: Emitter.Listener<E>, config?: Emitter.ListenerConfig)
    local wrappedFunction: Emitter.Listener<E>
    wrappedFunction = function(e: E)
        self:off(event, wrappedFunction)
        listener(e)
    end
    self:on(event, wrappedFunction, config)
end

function Emitter:emit(event: Emitter.Event)
    local listeners <const> = self._listeners[event.type]
    if listeners then
        local node = listeners.head
        while node do
            node.listener(event)
            node = node.next
        end
    end

    local node = self._forwarding.head
    while node do
        node.emitter:emit(event)
        node = node.next
    end
end

function Emitter:removeAllListeners(event: Emitter.Event)
    self._listeners[event] = nil
end

function Emitter:startForwarding(emitter: Emitter)
    local node <const>: EmitterNode = { emitter = emitter }
    LinkedList.append(self._forwarding, node as ListNode<any>)
end

function Emitter:stopForwarding(emitter: Emitter)
    local node = self._forwarding.head
    while node do
        if node.emitter == emitter then
            LinkedList.remove(self._forwarding, node as ListNode<any>)
            return
        end
        node = node.next
    end
end

return Emitter
