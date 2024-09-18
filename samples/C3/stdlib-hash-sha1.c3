// Source: https://github.com/c3lang/c3c/blob/master/lib/std/hash/sha1.c3
// Permalink: https://github.com/c3lang/c3c/blob/499c82b/lib/std/hash/sha1.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// Implementation was off Steve Reid's SHA-1 C implementation

module std::hash::sha1;
import std::bits;

struct Sha1
{
	uint[5] state;
	uint[2] count;
	char[64] buffer;
}

fn void Sha1.init(&self)
{
	// SHA1 initialization constants
	*self = {
		.state = {
			0x67452301,
			0xEFCDAB89,
			0x98BADCFE,
			0x10325476,
			0xC3D2E1F0
		}
	};
}

/**
 * @param [in] data
 * @require data.len <= uint.max
 **/
fn void Sha1.update(&self, char[] data)
{
	uint j = self.count[0];
	uint len = data.len;
	if ((self.count[0] += len << 3) < j) self.count[1]++;
	self.count[1] += len >> 29;
	j = (j >> 3) & 63;
	uint i;
	if (j + len > 63)
	{
		i = 64 - j;
		self.buffer[j..] = data[:i];
		sha1_transform(&self.state, &self.buffer);
		for (; i + 63 < len; i += 64)
		{
			sha1_transform(&self.state, &data[i]);
		}
		j = 0;
	}
	self.buffer[j:len - i] = data[i..];
}


fn char[20] Sha1.final(&self)
{
	char[8] finalcount;
	for (uint i = 0; i < 8; i++)
	{
		finalcount[i] = (char)((self.count[(i >= 4 ? 0 : 1)] >> ((3 - (i & 3)) * 8)) & 0xFF);
	}
	self.update(char[] { 0o200 });
	while ((self.count[0] & 504) != 448)
	{
		self.update(char[] { 0 });
	}

	self.update(&finalcount);
	char[20] digest;
	for (uint i = 0; i < 20; i++)
	{
		digest[i] = (char)((self.state[i >> 2] >> ((3 - (i & 3)) * 8)) & 0xFF);
	}

	// Clear mem
	mem::clear(self, Sha1.sizeof);
	finalcount = {};
	return digest;
}

union Long16 @local
{
	char[64] c;
	uint[16] l;
}

macro @blk(&block, i) @local
{
	return (block.l[i & 15] = (block.l[(i + 13) & 15] ^ block.l[(i + 8) & 15]
		^ block.l[(i + 2) & 15] ^ block.l[i & 15]).rotl(1));
}

macro @blk0(&block, i) @local
{
	$if env::BIG_ENDIAN:
		return block.l[i];
	$else
		return block.l[i] = (block.l[i].rotl(24) & 0xFF00FF00)
			| (block.l[i].rotl(8) & 0x00FF00FF);
	$endif
}

macro @r0(&block, v, &wref, x, y, &z, i) @local
{
	var w = *wref;
	*z += ((w & (x ^ y)) ^ y) + @blk0(*block, i) + 0x5A827999 + v.rotl(5);
	*wref = w.rotl(30);
}

macro @r1(&block, v, &wref, x, y, &z, i) @local
{
	var w = *wref;
	*z += ((w & (x ^ y)) ^ y) + @blk(*block, i) + 0x5A827999 + v.rotl(5);
	*wref = w.rotl(30);
}

macro @r2(&block, v, &wref, x, y, &z, i) @local
{
	var w = *wref;
	*z += (w ^ x ^ y) + @blk(*block, i) + 0x6ED9EBA1 + v.rotl(5);
	*wref = w.rotl(30);
}

macro @r3(&block, v, &wref, x, y, &z, i) @local
{
	var w = *wref;
	*z += (((w | x) & y) | (w & x)) + @blk(*block, i) + 0x8F1BBCDC + v.rotl(5);
	*wref = w.rotl(30);
}

macro @r4(&block, v, &wref, x, y, &z, i) @local
{
	var w = *wref;
	*z += (w ^ x ^ y) + @blk(*block, i) + 0xCA62C1D6 + v.rotl(5);
	*wref = w.rotl(30);
}

/**
 * @param [&inout] state
 * @param [&in] buffer
 **/
fn void sha1_transform(uint* state, char* buffer) @local
{
	Long16 block;
	block.c[..] = buffer[:64];
	uint a = state[0];
	uint b = state[1];
	uint c = state[2];
	uint d = state[3];
	uint e = state[4];
	@r0(block, a, b, c, d, e, 0);
	@r0(block, e, a, b, c, d, 1);
	@r0(block, d, e, a, b, c, 2);
	@r0(block, c, d, e, a, b, 3);
	@r0(block, b, c, d, e, a, 4);
	@r0(block, a, b, c, d, e, 5);
	@r0(block, e, a, b, c, d, 6);
	@r0(block, d, e, a, b, c, 7);
	@r0(block, c, d, e, a, b, 8);
	@r0(block, b, c, d, e, a, 9);
	@r0(block, a, b, c, d, e, 10);
	@r0(block, e, a, b, c, d, 11);
	@r0(block, d, e, a, b, c, 12);
	@r0(block, c, d, e, a, b, 13);
	@r0(block, b, c, d, e, a, 14);
	@r0(block, a, b, c, d, e, 15);
	@r1(block, e, a, b, c, d, 16);
	@r1(block, d, e, a, b, c, 17);
	@r1(block, c, d, e, a, b, 18);
	@r1(block, b, c, d, e, a, 19);
	@r2(block, a, b, c, d, e, 20);
	@r2(block, e, a, b, c, d, 21);
	@r2(block, d, e, a, b, c, 22);
	@r2(block, c, d, e, a, b, 23);
	@r2(block, b, c, d, e, a, 24);
	@r2(block, a, b, c, d, e, 25);
	@r2(block, e, a, b, c, d, 26);
	@r2(block, d, e, a, b, c, 27);
	@r2(block, c, d, e, a, b, 28);
	@r2(block, b, c, d, e, a, 29);
	@r2(block, a, b, c, d, e, 30);
	@r2(block, e, a, b, c, d, 31);
	@r2(block, d, e, a, b, c, 32);
	@r2(block, c, d, e, a, b, 33);
	@r2(block, b, c, d, e, a, 34);
	@r2(block, a, b, c, d, e, 35);
	@r2(block, e, a, b, c, d, 36);
	@r2(block, d, e, a, b, c, 37);
	@r2(block, c, d, e, a, b, 38);
	@r2(block, b, c, d, e, a, 39);
	@r3(block, a, b, c, d, e, 40);
	@r3(block, e, a, b, c, d, 41);
	@r3(block, d, e, a, b, c, 42);
	@r3(block, c, d, e, a, b, 43);
	@r3(block, b, c, d, e, a, 44);
	@r3(block, a, b, c, d, e, 45);
	@r3(block, e, a, b, c, d, 46);
	@r3(block, d, e, a, b, c, 47);
	@r3(block, c, d, e, a, b, 48);
	@r3(block, b, c, d, e, a, 49);
	@r3(block, a, b, c, d, e, 50);
	@r3(block, e, a, b, c, d, 51);
	@r3(block, d, e, a, b, c, 52);
	@r3(block, c, d, e, a, b, 53);
	@r3(block, b, c, d, e, a, 54);
	@r3(block, a, b, c, d, e, 55);
	@r3(block, e, a, b, c, d, 56);
	@r3(block, d, e, a, b, c, 57);
	@r3(block, c, d, e, a, b, 58);
	@r3(block, b, c, d, e, a, 59);
	@r4(block, a, b, c, d, e, 60);
	@r4(block, e, a, b, c, d, 61);
	@r4(block, d, e, a, b, c, 62);
	@r4(block, c, d, e, a, b, 63);
	@r4(block, b, c, d, e, a, 64);
	@r4(block, a, b, c, d, e, 65);
	@r4(block, e, a, b, c, d, 66);
	@r4(block, d, e, a, b, c, 67);
	@r4(block, c, d, e, a, b, 68);
	@r4(block, b, c, d, e, a, 69);
	@r4(block, a, b, c, d, e, 70);
	@r4(block, e, a, b, c, d, 71);
	@r4(block, d, e, a, b, c, 72);
	@r4(block, c, d, e, a, b, 73);
	@r4(block, b, c, d, e, a, 74);
	@r4(block, a, b, c, d, e, 75);
	@r4(block, e, a, b, c, d, 76);
	@r4(block, d, e, a, b, c, 77);
	@r4(block, c, d, e, a, b, 78);
	@r4(block, b, c, d, e, a, 79);
	state[0] += a;
	state[1] += b;
	state[2] += c;
	state[3] += d;
	state[4] += e;
	a = b = c = d = e = 0;
	buffer[:64] = 0;
}