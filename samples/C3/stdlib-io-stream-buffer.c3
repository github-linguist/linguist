// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/stream/buffer.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/io/stream/buffer.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;

struct ReadBuffer (InStream)
{
	InStream wrapped_stream;
	char[] bytes;
	usz read_idx;
	usz write_idx;
}

/**
 * Buffer reads from a stream.
 * @param [inout] self
 * @require bytes.len > 0
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn ReadBuffer* ReadBuffer.init(&self, InStream wrapped_stream, char[] bytes)
{
	*self = { .wrapped_stream = wrapped_stream, .bytes = bytes };
	return self;
}
fn String ReadBuffer.str_view(&self) @inline
{
	return (String)self.bytes[self.read_idx:self.write_idx - self.read_idx];
}

fn void! ReadBuffer.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) self.wrapped_stream.close()!;
}

fn usz! ReadBuffer.read(&self, char[] bytes) @dynamic
{
	if (self.read_idx == self.write_idx)
	{
		if (self.read_idx == 0 && bytes.len >= self.bytes.len)
		{
			// Read directly into the input buffer.
			return self.wrapped_stream.read(bytes)!;
		}
		self.refill()!;
	}
	usz n = min(self.write_idx - self.read_idx, bytes.len);
	bytes[:n] = self.bytes[self.read_idx:n];
	self.read_idx += n;
	return n;
}

fn char! ReadBuffer.read_byte(&self) @dynamic
{
	if (self.read_idx == self.write_idx) self.refill()!;
	if (self.read_idx == self.write_idx) return IoError.EOF?;
	char c = self.bytes[self.read_idx];
	self.read_idx++;
	return c;
}

fn void! ReadBuffer.refill(&self) @local @inline
{
	self.read_idx = 0;
	self.write_idx = self.wrapped_stream.read(self.bytes)!;
}

struct WriteBuffer (OutStream)
{
	OutStream wrapped_stream;
	char[] bytes;
	usz index;
}

/**
 * Buffer writes to a stream. Call `flush` when done writing to the buffer.
 * @param [inout] self
 * @require bytes.len > 0 "Non-empty buffer required"
 * @require self.bytes.len == 0 "Init may not run on already initialized data"
 **/
fn WriteBuffer* WriteBuffer.init(&self, OutStream wrapped_stream, char[] bytes)
{
	*self = { .wrapped_stream = wrapped_stream, .bytes = bytes };
	return self;
}

fn String WriteBuffer.str_view(&self) @inline
{
	return (String)self.bytes[:self.index];
}

fn void! WriteBuffer.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) return self.wrapped_stream.close();
}

fn void! WriteBuffer.flush(&self) @dynamic
{
	self.write_pending()!;
	if (&self.wrapped_stream.flush) self.wrapped_stream.flush()!;
}

fn usz! WriteBuffer.write(&self, char[] bytes) @dynamic
{
	usz n = self.bytes.len - self.index;
	if (bytes.len < n)
	{
		// Enough room in the buffer.
		self.bytes[self.index:bytes.len] = bytes[..];
		self.index += bytes.len;
		return bytes.len;
	}
	self.write_pending()!;
	if (bytes.len >= self.bytes.len)
	{
		// Write directly to the stream.
		return self.wrapped_stream.write(bytes);
	}
	// Buffer the data.
	self.bytes[:bytes.len] = bytes[..];
	self.index = bytes.len;
	return bytes.len;
}

fn void! WriteBuffer.write_byte(&self, char c) @dynamic
{
	usz n = self.bytes.len - self.index;
	if (n == 0) self.write_pending()!;
	self.bytes[0] = c;
	self.index = 1;
}

fn void! WriteBuffer.write_pending(&self) @local
{
	self.index -= self.wrapped_stream.write(self.bytes[:self.index])!;
	if (self.index != 0) return IoError.INCOMPLETE_WRITE?;
}