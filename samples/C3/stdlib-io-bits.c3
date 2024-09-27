// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/bits.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/io/bits.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;

struct BitReader
{
	InStream reader;
	uint bits;
	uint len;
}

fn void BitReader.init(&self, InStream byte_reader)
{
	*self = { .reader = byte_reader };
}

fn void BitReader.clear(&self) @inline
{
	self.len = 0;
}

/**
 * @require nbits <= 8
 * @require self.len + nbits <= uint.sizeof * 8
 **/
fn char! BitReader.read_bits(&self, uint nbits)
{
	uint bits = self.bits;
	if (self.len < nbits)
	{
		// New bits are pushed right.
		char c = self.reader.read_byte()!;
		bits <<= 8;
		bits |= c;
		self.bits = bits;
		self.len += 8;
	}
	self.len -= nbits;
	uint mask = (1 << nbits) - 1;
	return (char)((bits >> self.len) & mask);
}

struct BitWriter
{
	OutStream writer;
	uint bits;
	uint len;
}

fn void BitWriter.init(&self, OutStream byte_writer)
{
	*self = { .writer = byte_writer };
}

fn void! BitWriter.flush(&self)
{
	if (self.len == 0) return;
	uint bits = self.bits << (32 - self.len);
	uint n = (self.len + 7) / 8;
	char[4] buffer;
	bitorder::write(bits, &buffer, UIntBE);
	io::write_all(self.writer, buffer[:n])!;
	self.len = 0;
}

/**
 * @require nbits <= 8
 **/
fn void! BitWriter.write_bits(&self, uint bits, uint nbits)
{
	if (nbits == 0) return;
	uint n = self.len + nbits;
	uint to_write = n / 8;
	uint left = n % 8;
	if (to_write > 0)
	{
		ulong lbits;
		if (self.len > 0) lbits = (ulong)self.bits << (64 - self.len);
		lbits |= (ulong)(bits >> left) << (64 - (n - left));
		char[8] buffer;
		bitorder::write(lbits, &buffer, ULongBE);
		io::write_all(self.writer, buffer[:to_write])!;
	}
	self.bits <<= left;
	self.bits |= bits & ((1 << left) - 1);
	self.len = left;
}