// Source: https://github.com/c3lang/c3c/blob/master/lib/std/hash/adler32.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/hash/adler32.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::hash::adler32;

const uint ADLER_CONST @private = 65521;

struct Adler32
{
	uint a;
	uint b;
}

fn void Adler32.init(&self)
{
	*self = { 1, 0 };
}

fn void Adler32.updatec(&self, char c)
{
	self.a = (self.a + c) % ADLER_CONST;
	self.b = (self.b + self.a) % ADLER_CONST;
}

fn void Adler32.update(&self, char[] data)
{
	uint a = self.a;
	uint b = self.b;
	foreach (char x : data)
	{
		a = (a + x) % ADLER_CONST;
		b = (b + a) % ADLER_CONST;
	}
	*self = { a, b };
}

fn uint Adler32.final(&self)
{
	return (self.b << 16) | self.a;
}

fn uint encode(char[] data)
{
	uint a = 1;
	uint b = 0;
	foreach (char x : data)
	{
		a = (a + x) % ADLER_CONST;
		b = (b + a) % ADLER_CONST;
	}
	return (b << 16) | a;
}