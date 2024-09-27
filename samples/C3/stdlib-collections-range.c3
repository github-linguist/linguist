// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/range.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/collections/range.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

/**
 * @require Type.is_ordered : "The type must be ordered"
 **/
module std::collections::range(<Type>);
import std::io;

struct Range (Printable)
{
	Type start;
	Type end;
}

fn usz Range.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start) + 1;
}

fn bool Range.contains(&self, Type value) @inline
{
	return value >= self.start && value <= self.end;
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type Range.get(&self, usz index) @operator([])
{
	return (Type)(self.start + (usz)index);
}

fn String Range.to_new_string(&self, Allocator allocator = allocator::heap()) @dynamic @deprecated
{
	return string::format("[%s..%s]", self.start, self.end, allocator: allocator);
}

fn String Range.to_string(&self, Allocator allocator) @dynamic
{
	return string::format("[%s..%s]", self.start, self.end, allocator: allocator);
}

fn String Range.to_tstring(&self)
{
	return self.to_string(allocator::temp());
}

fn usz! Range.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..%s]", self.start, self.end)!;
}

struct ExclusiveRange (Printable)
{
	Type start;
	Type end;
}

fn usz ExclusiveRange.len(&self) @operator(len)
{
	if (self.end < self.start) return 0;
	return (usz)(self.end - self.start);
}

fn bool ExclusiveRange.contains(&self, Type value) @inline
{
	return value >= self.start && value < self.end;
}

fn usz! ExclusiveRange.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("[%s..<%s]", self.start, self.end)!;
}

fn String ExclusiveRange.to_new_string(&self, Allocator allocator = null) @dynamic
{
	return self.to_string(allocator ?: allocator::heap());
}

fn String ExclusiveRange.to_string(&self, Allocator allocator) @dynamic
{
	return string::format("[%s..<%s]", self.start, self.end, allocator: allocator);
}

fn String ExclusiveRange.to_tstring(&self)
{
	return self.to_new_string(allocator::temp());
}

/**
 * @require index < self.len() : "Can't index into an empty range"
 **/
fn Type ExclusiveRange.get(&self, usz index) @operator([])
{
	return (Type)(self.start + index);
}
