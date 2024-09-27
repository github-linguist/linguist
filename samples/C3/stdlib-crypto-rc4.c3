// Source: https://github.com/c3lang/c3c/blob/master/lib/std/crypto/rc4.c3
// Permalink: https://github.com/c3lang/c3c/blob/c84bc8a/lib/std/crypto/rc4.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::crypto::rc4;
// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

struct Rc4
{
	uint i, j;
	char[256] state;
}

/**
 * Initialize the RC4 state.
 *
 * @param [in] key "The key to use"
 * @require key.len > 0 "The key must be at least 1 byte long"
 **/
fn void Rc4.init(&self, char[] key)
{
	// Init the state matrix
	foreach (char i, &c : self.state) *c = i;
	for (int i = 0, int j = 0; i < 256; i++)
	{
		j = (j + self.state[i] + key[i % key.len]) & 0xFF;
		@swap(self.state[i], self.state[j]);
	}
	self.i = 0;
	self.j = 0;
}

/**
 * Run a single pass of en/decryption using a particular key.
 * @param [in] key
 * @param [inout] data
 **/
fn void crypt(char[] key, char[] data)
{
	Rc4 rc4;
	rc4.init(key);
	rc4.crypt(data, data);
}

/**
 * Encrypt or decrypt a sequence of bytes.
 *
 * @param [in] in "The input"
 * @param [out] out "The output"
 * @require in.len <= out.len "Output would overflow"
 **/
fn void Rc4.crypt(&self, char[] in, char[] out)
{
	uint i = self.i;
	uint j = self.j;
	char* state = &self.state;
	isz len = in.len;
	foreach (idx, c : in)
	{
		i = (i + 1) & 0xFF;
		j = (j + state[i]) & 0xFF;
		@swap(state[i], state[j]);
		out[idx] = in[idx] ^ state[(state[i] + state[j]) & 0xFF];
	}
	self.i = i;
	self.j = j;
}

/**
 * Clear the rc4 state.
 *
 * @param [&out] self "The RC4 State"
 **/
fn void Rc4.destroy(&self)
{
	*self = {};
}
