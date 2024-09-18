// Source: https://github.com/c3lang/c3c/blob/master/lib/std/hash/fnv64a.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/hash/fnv64a.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::fnv64a;

distinct Fnv64a = ulong;

const FNV64A_START @private = 0xcbf29ce484222325;
const FNV64A_MUL @private = 0x00000100000001b3;

macro void @update(ulong* &h, char x) @private => *h = (*h * FNV64A_MUL) ^ x;

fn void Fnv64a.init(&self)
{
	*self = FNV64A_START;
}

fn void Fnv64a.update(&self, char[] data)
{
	ulong h = (ulong)*self;
	foreach (char x : data)
	{
		@update(h, x);
	}
	*self = (Fnv64a)h;
}

macro void Fnv64a.update_char(&self, char c)
{
	@update(*self, x);
}

fn ulong encode(char[] data)
{
	ulong h = FNV64A_START;
	foreach (char x : data)
	{
		@update(h, x);
	}
	return h;
}