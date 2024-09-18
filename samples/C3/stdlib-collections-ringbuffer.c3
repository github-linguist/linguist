// Source: https://github.com/c3lang/c3c/blob/master/lib/std/collections/ringbuffer.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/collections/ringbuffer.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::collections::ringbuffer(<Type, SIZE>);

struct RingBuffer
{
	Type[SIZE] buf;
	usz written;
	usz head;
}

fn void RingBuffer.init(&self) @inline
{
	*self = {};
}

fn void RingBuffer.push(&self, Type c)
{
	if (self.written < SIZE)
	{
		self.buf[self.written] = c;
		self.written++;
	}
	else
	{
		self.buf[self.head] = c;
		self.head = (self.head + 1) % SIZE;
	}
}

fn Type RingBuffer.get(&self, usz index) @operator([])
{
	index %= SIZE;
	usz avail = SIZE - self.head;
	if (index < avail)
	{
		return self.buf[self.head + index];
	}
	return self.buf[index - avail];
}

fn Type! RingBuffer.pop(&self)
{
	switch
	{
		case self.written == 0:
			return SearchResult.MISSING?;
		case self.written < SIZE:
			self.written--;
			return self.buf[self.written];
		default:
			self.head = (self.head - 1) % SIZE;
			return self.buf[self.head];
	}
}

fn usz RingBuffer.read(&self, usz index, Type[] buffer)
{
	index %= SIZE;
	if (self.written < SIZE)
	{
		if (index >= self.written) return 0;
		usz end = self.written - index;
		usz n = min(end, buffer.len);
		buffer[:n] = self.buf[index:n];
		return n;
	}
	usz end = SIZE - self.head;
	if (index >= end)
	{
		index -= end;
		if (index >= self.head) return 0;
		usz n = min(self.head - index, buffer.len);
		buffer[:n] = self.buf[index:n];
		return n;
	}
	if (buffer.len <= SIZE - index)
	{
		usz n = buffer.len;
		buffer[:n] = self.buf[self.head + index:n];
		return n;
	}
	usz n1 = SIZE - index;
	buffer[:n1] = self.buf[self.head + index:n1];
	buffer = buffer[n1..];
	index -= n1;
	usz n2 = min(self.head - index, buffer.len);
	buffer[:n2] = self.buf[index:n2];
	return n1 + n2;
}

fn void RingBuffer.write(&self, Type[] buffer)
{
	usz i;
	while (self.written < SIZE && i < buffer.len)
	{
		self.buf[self.written] = buffer[i++];
		self.written++;
	}
	foreach (c : buffer[i..])
	{
		self.buf[self.head] = c;
		self.head = (self.head + 1) % SIZE;
	}
}