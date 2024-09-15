// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/enummap.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/collections/enummap.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::collections::enummap(<Enum, ValueType>);
import std::io;
struct EnumMap (Printable)
{
	ValueType[Enum.len] values;
}

fn void EnumMap.init(&self, ValueType init_value)
{
	foreach (&a : self.values)
	{
		*a = init_value;
	}
}

fn usz! EnumMap.to_format(&self, Formatter* formatter) @dynamic
{
	usz n = formatter.print("{ ")!;
	foreach (i, &value : self.values)
	{
		if (i != 0) formatter.print(", ")!;
		n += formatter.printf("%s: %s", (Enum)i, *value)!;
	}
	n += formatter.print(" }")!;
	return n;
}

fn String EnumMap.to_string(&self, Allocator allocator) @dynamic
{
	return string::format("%s", *self, allocator: allocator);
}

fn String EnumMap.to_new_string(&self, Allocator allocator = null) @dynamic
{
	return string::format("%s", *self, allocator: allocator ?: allocator::heap());
}

fn String EnumMap.to_tstring(&self) @dynamic
{
	return string::tformat("%s", *self);
}

/**
 * @return "The total size of this map, which is the same as the number of enum values"
 * @pure
 **/
fn usz EnumMap.len(&self) @operator(len) @inline
{
	return self.values.len;
}

/**
 * @return "Retrieve a value given the underlying enum, if there is no entry, then the zero value for the value is returned."
 **/
fn ValueType EnumMap.get(&self, Enum key) @operator([]) @inline
{
	return self.values[key.ordinal];
}

fn ValueType* EnumMap.get_ref(&self, Enum key) @operator(&[]) @inline
{
	return &self.values[key.ordinal];
}

fn void EnumMap.set(&self, Enum key, ValueType value) @operator([]=) @inline
{
	self.values[key.ordinal] = value;
}