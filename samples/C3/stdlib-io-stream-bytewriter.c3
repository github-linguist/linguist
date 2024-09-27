// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/stream/bytewriter.c3
// Permalink: https://github.com/c3lang/c3c/blob/7664d05/lib/std/io/stream/bytewriter.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;
import std::math;

struct ByteWriter (OutStream)
{
	char[] bytes;
	usz index;
	Allocator allocator;
}

/**
 * @param [&inout] self
 * @param [&inout] allocator
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 * @ensure (bool)allocator, self.index == 0
 **/
fn ByteWriter* ByteWriter.new_init(&self, Allocator allocator = allocator::heap())
{
	*self = { .bytes = {}, .allocator = allocator };
	return self;
}

/**
 * @param [&inout] self
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 * @ensure self.index == 0
 **/
fn ByteWriter* ByteWriter.temp_init(&self)
{
	return self.new_init(allocator::temp()) @inline;
}

fn ByteWriter* ByteWriter.init_with_buffer(&self, char[] data)
{
	*self = { .bytes = data, .allocator = null };
	return self;
}

fn void! ByteWriter.destroy(&self) @dynamic
{
	if (!self.allocator) return;
	if (void* ptr = self.bytes.ptr) allocator::free(self.allocator, ptr);
	*self = { };
}

fn String ByteWriter.str_view(&self) @inline
{
	return (String)self.bytes[:self.index];
}

fn void! ByteWriter.ensure_capacity(&self, usz len) @inline
{
	if (self.bytes.len > len) return;
	if (!self.allocator) return IoError.OUT_OF_SPACE?;
	if (len < 16) len = 16;
	usz new_capacity = math::next_power_of_2(len);
	char* new_ptr = allocator::realloc_try(self.allocator, self.bytes.ptr, new_capacity)!;
	self.bytes = new_ptr[:new_capacity];
}

fn usz! ByteWriter.write(&self, char[] bytes) @dynamic
{
	self.ensure_capacity(self.index + bytes.len)!;
	mem::copy(&self.bytes[self.index], bytes.ptr, bytes.len);
	self.index += bytes.len;
	return bytes.len;
}

fn void! ByteWriter.write_byte(&self, char c) @dynamic
{
	self.ensure_capacity(self.index + 1)!;
	self.bytes[self.index++] = c;
}

/**
 * @param [&inout] self
 * @param reader
 **/
fn usz! ByteWriter.read_from(&self, InStream reader) @dynamic
{
	usz start_index = self.index;
	if (&reader.available)
	{
		while (usz available = reader.available()!)
		{
			self.ensure_capacity(self.index + available)!;
			usz read = reader.read(self.bytes[self.index..])!;
			self.index += read;
		}
		return self.index - start_index;
	}
	if (self.bytes.len == 0)
	{
		self.ensure_capacity(16)!;
	}
	while (true)
	{
		// See how much we can read.
		usz len_to_read = self.bytes.len - self.index;
		// Less than 16 bytes? Double the capacity
		if (len_to_read < 16)
		{
			self.ensure_capacity(self.bytes.len * 2)!;
			len_to_read = self.bytes.len - self.index;
		}
		// Read into the rest of the buffer
		usz read = reader.read(self.bytes[self.index..])!;
		self.index += read;
		// Ok, we reached the end.
		if (read < len_to_read) return self.index - start_index;
		// Otherwise go another round
	}
}
