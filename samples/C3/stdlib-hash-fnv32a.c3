// Source: https://github.com/c3lang/c3c/blob/master/lib/std/hash/fnv32a.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/hash/fnv32a.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::fnv32a;

distinct Fnv32a = uint;

const FNV32A_START @private = 0x811c9dc5;
const FNV32A_MUL @private = 0x01000193;

macro void @update(uint* &h, char x) @private => *h = (*h * FNV32A_MUL) ^ x;

fn void Fnv32a.init(&self)
{
	*self = FNV32A_START;
}

fn void Fnv32a.update(&self, char[] data)
{
	uint h = (uint)*self;
	foreach (char x : data)
	{
		@update(h, x);
	}
	*self = (Fnv32a)h;
}

macro void Fnv32a.update_char(&self, char c)
{
	@update(*self, x);
}

fn uint encode(char[] data)
{
	uint h = FNV32A_START;
	foreach (char x : data)
	{
		@update(h, x);
	}
	return h;
}