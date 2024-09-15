// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/stream/limitreader.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/io/stream/limitreader.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;

struct LimitReader (InStream)
{
	InStream wrapped_stream;
	usz limit;
}

/**
 * @param [&inout] wrapped_stream "The stream to read from"
 * @param limit "The max limit to read"
 **/
fn LimitReader* LimitReader.init(&self, InStream wrapped_stream, usz limit)
{
	*self = { .wrapped_stream = wrapped_stream, .limit = limit };
	return self;
}

fn void! LimitReader.close(&self) @dynamic
{
	if (&self.wrapped_stream.close) return self.wrapped_stream.close();
}


fn usz! LimitReader.read(&self, char[] bytes) @dynamic
{
	if (self.limit == 0) return IoError.EOF?;
	usz m = min(bytes.len, self.limit);
	usz n = self.wrapped_stream.read(bytes[:m])!;
	self.limit -= n;
	return n;
}

fn char! LimitReader.read_byte(&self) @dynamic
{
	if (self.limit == 0) return IoError.EOF?;
	defer try self.limit--;
	return self.wrapped_stream.read_byte();
}

fn usz! LimitReader.available(&self) @inline @dynamic
{
	return self.limit;
}