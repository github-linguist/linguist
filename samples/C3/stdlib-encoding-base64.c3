// Source: https://github.com/c3lang/c3c/blob/master/lib/std/encoding/base64.c3
// Permalink: https://github.com/c3lang/c3c/blob/d5aebb4/lib/std/encoding/base64.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::encoding::base64;
import std::core::bitorder;

// The implementation is based on https://www.rfc-editor.org/rfc/rfc4648
// Specifically this section:
// https://www.rfc-editor.org/rfc/rfc4648#section-4

const STD_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const URL_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

const MASK @private = 0b111111;

struct Base64Encoder
{
	int padding;
	String alphabet;
}

fault Base64Error
{
	DUPLICATE_IN_ALPHABET,
	PADDING_IN_ALPHABET,
	DESTINATION_TOO_SMALL,
	INVALID_PADDING,
	INVALID_CHARACTER,
}

/**
 * @param alphabet "The alphabet used for encoding."
 * @param padding "Set to a negative value to disable padding."
 * @require alphabet.len == 64
 * @require padding < 256
 * @return! Base64Error.DUPLICATE_IN_ALPHABET, Base64Error.PADDING_IN_ALPHABET
 **/
fn void! Base64Encoder.init(&self, String alphabet, int padding = '=')
{
	check_alphabet(alphabet, padding)!;
	*self = { .padding = padding, .alphabet = alphabet };
}

/**
 * Calculate the size of the encoded data.
 * @param n "Size of the input to be encoded."
 * @return "The size of the input once encoded."
 **/
fn usz Base64Encoder.encode_len(&self, usz n)
{
	if (self.padding >= 0) return (n + 2) / 3 * 4;
	usz trailing = n % 3;
	return n / 3 * 4 + (trailing * 4 + 2) / 3;
}

/**
 * Encode the content of src into dst, which must be properly sized.
 * @param src "The input to be encoded."
 * @param dst "The encoded input."
 * @return "The encoded size."
 * @return! Base64Error.DESTINATION_TOO_SMALL
 **/
fn usz! Base64Encoder.encode(&self, char[] src, char[] dst)
{
	if (src.len == 0) return 0;
	usz dn = self.encode_len(src.len);
	if (dst.len < dn) return Base64Error.DESTINATION_TOO_SMALL?;
	usz trailing = src.len % 3;
	char[] src3 = src[:^trailing];

	while (src3.len > 0)
	{
		uint group = (uint)src3[0] << 16 | (uint)src3[1] << 8 | (uint)src3[2];
		dst[0] = self.alphabet[group >> 18 & MASK];
		dst[1] = self.alphabet[group >> 12 & MASK];
		dst[2] = self.alphabet[group >> 6 & MASK];
		dst[3] = self.alphabet[group & MASK];
		dst = dst[4..];
		src3 = src3[3..];
	}

	// Encode the remaining bytes according to:
	// https://www.rfc-editor.org/rfc/rfc4648#section-3.5
	switch (trailing)
	{
		case 1:
			uint group = (uint)src[^1] << 16;
			dst[0] = self.alphabet[group >> 18 & MASK];
			dst[1] = self.alphabet[group >> 12 & MASK];
			if (self.padding >= 0)
			{
				char pad = (char)self.padding;
				dst[2] = pad;
				dst[3] = pad;
			}
		case 2:
			uint group = (uint)src[^2] << 16 | (uint)src[^1] << 8;
			dst[0] = self.alphabet[group >> 18 & MASK];
			dst[1] = self.alphabet[group >> 12 & MASK];
			dst[2] = self.alphabet[group >> 6 & MASK];
			if (self.padding >= 0)
			{
				char pad = (char)self.padding;
				dst[3] = pad;
			}
	}
	return dn;
}

struct Base64Decoder
{
	int padding;
	String alphabet;
	char[256] reverse;
	char invalid;
}

/**
 * @param alphabet "The alphabet used for encoding."
 * @param padding "Set to a negative value to disable padding."
 * @require alphabet.len == 64
 * @require padding < 256
 * @return! Base64Error.DUPLICATE_IN_ALPHABET, Base64Error.PADDING_IN_ALPHABET
 **/
fn void! Base64Decoder.init(&self, String alphabet, int padding = '=')
{
	check_alphabet(alphabet, padding)!;
	*self = { .padding = padding, .alphabet = alphabet };

	bool[256] checked;
	foreach (i, c : alphabet)
	{
		checked[c] = true;
		self.reverse[c] = (char)i;
	}
	if (padding < 0)
	{
		self.invalid = 255;
		return;
	}
	// Find a character for invalid neither in the alphabet nor equal to the padding.
	char pad = (char)padding;
	foreach (i, ok : checked)
	{
		if (!ok && (char)i != pad)
		{
			self.invalid = (char)i;
			break;
		}
	}
}

/**
 * Calculate the size of the decoded data.
 * @param n "Size of the input to be decoded."
 * @return "The size of the input once decoded."
 * @return! Base64Error.INVALID_PADDING
 **/
fn usz! Base64Decoder.decode_len(&self, usz n)
{
	usz dn = n / 4 * 3;
	usz trailing = n % 4;
	if (self.padding >= 0)
	{
		if (trailing != 0) return Base64Error.INVALID_PADDING?;
		// source size is multiple of 4
	}
	else
	{
		if (trailing == 1) return Base64Error.INVALID_PADDING?;
		dn += trailing * 3 / 4;
	}
	return dn;
}

/**
 * Decode the content of src into dst, which must be properly sized.
 * @param src "The input to be decoded."
 * @param dst "The decoded input."
 * @return "The decoded size."
 * @return! Base64Error.DESTINATION_TOO_SMALL, Base64Error.INVALID_PADDING, Base64Error.INVALID_CHARACTER
 **/
fn usz! Base64Decoder.decode(&self, char[] src, char[] dst)
{
	if (src.len == 0) return 0;
	usz dn = self.decode_len(src.len)!;
	if (dst.len < dn) return Base64Error.DESTINATION_TOO_SMALL?;

	usz trailing = src.len % 4;
	char[] src4 = src;
	switch
	{
		case self.padding < 0:
			src4 = src[:^trailing];
		default:
			// If there is padding, keep the last 4 bytes for later.
			// NB. src.len >= 4 as decode_len passed
			trailing = 4;
			char pad = (char)self.padding;
			if (src[^1] == pad) src4 = src[:^4];
	}
	while (src4.len > 0)
	{
		char c0 = self.reverse[src4[0]];
		char c1 = self.reverse[src4[1]];
		char c2 = self.reverse[src4[2]];
		char c3 = self.reverse[src4[3]];
		switch (self.invalid)
		{
			case c0:
			case c1:
			case c2:
			case c3:
				return Base64Error.INVALID_CHARACTER?;
		}
		uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6 | (uint)c3;
		dst[0] = (char)(group >> 16);
		dst[1] = (char)(group >> 8);
		dst[2] = (char)group;
		dst = dst[3..];
		src4 = src4[4..];
	}

	if (trailing == 0) return dn;

	src = src[^trailing..];
	char c0 = self.reverse[src[0]];
	char c1 = self.reverse[src[1]];
	if (c0 == self.invalid || c1 == self.invalid) return Base64Error.INVALID_PADDING?;
	if (self.padding < 0)
	{
		switch (src.len)
		{
			case 2:
				uint group = (uint)c0 << 18 | (uint)c1 << 12;
				dst[0] = (char)(group >> 16);
			case 3:
				char c2 = self.reverse[src[2]];
				if (c2 == self.invalid) return Base64Error.INVALID_CHARACTER?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6;
				dst[0] = (char)(group >> 16);
				dst[1] = (char)(group >> 8);
		}
	}
	else
	{
		// Valid paddings are:
		// 2: xx==
		// 1: xxx=
		char pad = (char)self.padding;
		switch (pad)
		{
			case src[2]:
				if (src[3] != pad) return Base64Error.INVALID_PADDING?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12;
				dst[0] = (char)(group >> 16);
				dn -= 2;
			case src[3]:
				char c2 = self.reverse[src[2]];
				if (c2 == self.invalid) return Base64Error.INVALID_CHARACTER?;
				uint group = (uint)c0 << 18 | (uint)c1 << 12 | (uint)c2 << 6;
				dst[0] = (char)(group >> 16);
				dst[1] = (char)(group >> 8);
				dn -= 1;
		}
	}
	return dn;
}

// Make sure that all bytes in the alphabet are unique and
// the padding is not present in the alphabet.
fn void! check_alphabet(String alphabet, int padding) @local
{
	bool[256] checked;
	if (padding < 0)
	{
		foreach (c : alphabet)
		{
			if (checked[c]) return Base64Error.DUPLICATE_IN_ALPHABET?;
			checked[c] = true;
		}
		return;
	}
	char pad = (char)padding;
	foreach (c : alphabet)
	{
		if (c == pad) return Base64Error.PADDING_IN_ALPHABET?;
		if (checked[c]) return Base64Error.DUPLICATE_IN_ALPHABET?;
		checked[c] = true;
	}
}