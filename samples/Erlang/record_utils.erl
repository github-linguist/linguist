%% This is auto generated file. Please don't edit it

-module(record_utils).
-compile(export_all).
-include("messages.hrl").

fields(abstract_message) ->
	["clientId", "destination", "messageId", "timestamp", "timeToLive", "headers", "body"];

fields(async_message) ->
	fields(abstract_message) ++ ["correlationId", "correlationIdBytes"].

fields_atom(abstract_message) ->
	lists:flatten([clientId, destination, messageId, timestamp, timeToLive, headers, body]);

fields_atom(async_message) ->
	lists:flatten([fields_atom(abstract_message), correlationId, correlationIdBytes]).

get(Obj, body) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.body};

get(Obj, clientId) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.clientId};

get(Obj, destination) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.destination};

get(Obj, headers) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.headers};

get(Obj, messageId) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.messageId};

get(Obj, timeToLive) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.timeToLive};

get(Obj, timestamp) when is_record(Obj, abstract_message) ->
	{ok, Obj#abstract_message.timestamp};

get(Obj, correlationId) when is_record(Obj, async_message) ->
	{ok, Obj#async_message.correlationId};

get(Obj, correlationIdBytes) when is_record(Obj, async_message) ->
	{ok, Obj#async_message.correlationIdBytes};

get(Obj, parent) when is_record(Obj, async_message) ->
	{ok, Obj#async_message.parent};

get(Obj, ParentProperty) when is_record(Obj, async_message) and is_atom(ParentProperty) ->
	get(Obj#async_message.parent, ParentProperty).

set(Obj, body, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{body = Value},
	{ok, NewObj, {body, Value}};

set(Obj, clientId, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{clientId = Value},
	{ok, NewObj, {clientId, Value}};

set(Obj, destination, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{destination = Value},
	{ok, NewObj, {destination, Value}};

set(Obj, headers, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{headers = Value},
	{ok, NewObj, {headers, Value}};

set(Obj, messageId, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{messageId = Value},
	{ok, NewObj, {messageId, Value}};

set(Obj, timeToLive, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{timeToLive = Value},
	{ok, NewObj, {timeToLive, Value}};

set(Obj, timestamp, Value) when is_record(Obj, abstract_message) ->
	NewObj = Obj#abstract_message{timestamp = Value},
	{ok, NewObj, {timestamp, Value}};

set(Obj, correlationId, Value) when is_record(Obj, async_message) ->
	NewObj = Obj#async_message{correlationId = Value},
	{ok, NewObj, {correlationId, Value}};

set(Obj, correlationIdBytes, Value) when is_record(Obj, async_message) ->
	NewObj = Obj#async_message{correlationIdBytes = Value},
	{ok, NewObj, {correlationIdBytes, Value}};

set(Obj, parent, Value) when is_record(Obj, async_message) and is_record(Value, abstract_message) ->
	NewObj = Obj#async_message{parent = Value},
	{ok, NewObj, {parent, Value}};

set(Obj, ParentProperty, Value) when is_record(Obj, async_message) and is_atom(ParentProperty) ->
	{ok, NewParentObject, _} = set(Obj#async_message.parent, ParentProperty, Value),
	set(Obj, parent, NewParentObject).

type(Obj) when is_record(Obj, abstract_message) -> abstract_message;

type(Obj) when is_record(Obj, async_message) -> async_message;

type(_) -> undefined.