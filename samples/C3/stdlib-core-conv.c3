// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/conv.c3
// Permalink: https://github.com/c3lang/c3c/blob/0b971c2/lib/std/core/conv.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::string::conv;

const uint UTF16_SURROGATE_OFFSET @private = 0x10000;
const uint UTF16_SURROGATE_GENERIC_MASK @private = 0xF800;
const uint UTF16_SURROGATE_GENERIC_VALUE @private = 0xD800;
const uint UTF16_SURROGATE_MASK @private = 0xFC00;
const uint UTF16_SURROGATE_CODEPOINT_MASK @private = 0x03FF;
const uint UTF16_SURROGATE_BITS @private = 10;
const uint UTF16_SURROGATE_LOW_VALUE @private = 0xDC00;
const uint UTF16_SURROGATE_HIGH_VALUE @private = 0xD800;

/**
 * @param c `The utf32 codepoint to convert`
 * @param [out] output `the resulting buffer`
 **/
fn usz! char32_to_utf8(Char32 c, char[] output)
{
	if (!output.len) return UnicodeResult.CONVERSION_FAILED?;
	switch (true)
	{
		case c <= 0x7f:
			output[0] = (char)c;
			return 1;
		case c <= 0x7ff:
			if (output.len < 2) return UnicodeResult.CONVERSION_FAILED?;
			output[0] = (char)(0xC0 | c >> 6);
			output[1] = (char)(0x80 | (c & 0x3F));
			return 2;
		case c <= 0xffff:
			if (output.len < 3) return UnicodeResult.CONVERSION_FAILED?;
			output[0] = (char)(0xE0 | c >> 12);
			output[1] = (char)(0x80 | (c >> 6 & 0x3F));
			output[2] = (char)(0x80 | (c & 0x3F));
			return 3;
		case c <= 0x10ffff:
			if (output.len < 4) return UnicodeResult.CONVERSION_FAILED?;
			output[0] = (char)(0xF0 | c >> 18);
			output[1] = (char)(0x80 | (c >> 12 & 0x3F));
			output[2] = (char)(0x80 | (c >> 6 & 0x3F));
			output[3] = (char)(0x80 | (c & 0x3F));
			return 4;
		default:
			// 0x10FFFF and above is not defined.
			return UnicodeResult.CONVERSION_FAILED?;
	}
}

/**
 * Convert a code pointer into 1-2 UTF16 characters.
 *
 * @param c `The character to convert.`
 * @param [inout] output `the resulting UTF16 buffer to write to.`
 **/
fn void char32_to_utf16_unsafe(Char32 c, Char16** output)
{
	if (c < UTF16_SURROGATE_OFFSET)
	{
		(*output)++[0] = (Char16)c;
		return;
	}
	c -= UTF16_SURROGATE_OFFSET;
	Char16 low = (Char16)(UTF16_SURROGATE_LOW_VALUE | (c & UTF16_SURROGATE_CODEPOINT_MASK));
	c >>= UTF16_SURROGATE_BITS;
	Char16 high = (Char16)(UTF16_SURROGATE_HIGH_VALUE | (c & UTF16_SURROGATE_CODEPOINT_MASK));
	(*output)++[0] = (Char16)high;
	(*output)++[0] = (Char16)low;
}

/**
 * Convert 1-2 UTF16 data points into UTF8.
 *
 * @param [in] ptr `The UTF16 data to convert.`
 * @param [inout] available `amount of UTF16 data available.`
 * @param [inout] output `the resulting utf8 buffer to write to.`
 **/
fn void! char16_to_utf8_unsafe(Char16 *ptr, usz *available, char** output)
{
	Char16 high = *ptr;
	if (high & UTF16_SURROGATE_GENERIC_MASK != UTF16_SURROGATE_GENERIC_VALUE)
	{
		char32_to_utf8_unsafe(high, output);
		*available = 1;
		return;
	}
	// Low surrogate first is an error
	if (high & UTF16_SURROGATE_MASK != UTF16_SURROGATE_HIGH_VALUE) return UnicodeResult.INVALID_UTF16?;

	// Unmatched high surrogate is an error
	if (*available == 1) return UnicodeResult.INVALID_UTF16?;

	Char16 low = ptr[1];

	// Unmatched high surrogate, invalid
	if (low & UTF16_SURROGATE_MASK != UTF16_SURROGATE_LOW_VALUE) return UnicodeResult.INVALID_UTF16?;

	// The high bits of the codepoint are the value bits of the high surrogate
	// The low bits of the codepoint are the value bits of the low surrogate
	Char32 uc = (high & UTF16_SURROGATE_CODEPOINT_MASK) << UTF16_SURROGATE_BITS
				| (low & UTF16_SURROGATE_CODEPOINT_MASK) + UTF16_SURROGATE_OFFSET;
	char32_to_utf8_unsafe(uc, output);
	*available = 2;
}
/**
 * @param c `The utf32 codepoint to convert`
 * @param [inout] output `the resulting buffer`
 **/
fn usz char32_to_utf8_unsafe(Char32 c, char** output)
{
	switch
	{
		case c <= 0x7f:
			(*output)++[0] = (char)c;
			return 1;
		case c <= 0x7ff:
			(*output)++[0] = (char)(0xC0 | c >> 6);
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 2;
		case c <= 0xffff:
			(*output)++[0] = (char)(0xE0 | c >> 12);
			(*output)++[0] = (char)(0x80 | (c >> 6 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 3;
		default:
			(*output)++[0] = (char)(0xF0 | c >> 18);
			(*output)++[0] = (char)(0x80 | (c >> 12 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c >> 6 & 0x3F));
			(*output)++[0] = (char)(0x80 | (c & 0x3F));
			return 4;
	}
}

/**
 * @param [in] ptr `pointer to the first character to parse`
 * @param [inout] size `Set to max characters to read, set to characters read`
 * @return `the parsed 32 bit codepoint`
 **/
fn Char32! utf8_to_char32(char* ptr, usz* size)
{
	usz max_size = *size;
	if (max_size < 1) return UnicodeResult.INVALID_UTF8?;
	char c = (ptr++)[0];

	if ((c & 0x80) == 0)
	{
		*size = 1;
		return c;
	}
	if ((c & 0xE0) == 0xC0)
	{
		if (max_size < 2) return UnicodeResult.INVALID_UTF8?;
		*size = 2;
		Char32 uc = (c & 0x1F) << 6;
		c = *ptr;
		// Overlong sequence or invalid second.
		if (!uc || c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
		return uc + c & 0x3F;
	}
	if ((c & 0xF0) == 0xE0)
	{
		if (max_size < 3) return UnicodeResult.INVALID_UTF8?;
		*size = 3;
		Char32 uc = (c & 0x0F) << 12;
		c = ptr++[0];
		if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
		uc += (c & 0x3F) << 6;
		c = ptr++[0];
		// Overlong sequence or invalid last
		if (!uc || c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
		return uc + c & 0x3F;
	}
	if (max_size < 4) return UnicodeResult.INVALID_UTF8?;
	if ((c & 0xF8) != 0xF0) return UnicodeResult.INVALID_UTF8?;
	*size = 4;
	Char32 uc = (c & 0x07) << 18;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
	uc += (c & 0x3F) << 12;
	c = ptr++[0];
	if (c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
	uc += (c & 0x3F) << 6;
	c = ptr++[0];
	// Overlong sequence or invalid last
	if (!uc || c & 0xC0 != 0x80) return UnicodeResult.INVALID_UTF8?;
	return uc + c & 0x3F;
}

/**
 * @param utf8 `An UTF-8 encoded slice of bytes`
 * @return `the number of encoded code points`
 **/
fn usz utf8_codepoints(String utf8)
{
	usz len = 0;
	foreach (char c : utf8)
	{
		if (c & 0xC0 != 0x80) len++;
	}
	return len;
}

/**
 * Calculate the UTF8 length required to encode an UTF32 array.
 * @param [in] utf32 `the utf32 data to calculate from`
 * @return `the length of the resulting UTF8 array`
 **/
fn usz utf8len_for_utf32(Char32[] utf32)
{
	usz len = 0;
	foreach (Char32 uc : utf32)
	{
		switch (true)
		{
			case uc <= 0x7f:
				len++;
			case uc <= 0x7ff:
				len += 2;
			case uc <= 0xffff:
				len += 3;
			default:
				len += 4;
		}
	}
	return len;
}

/**
 * Calculate the UTF8 length required to encode an UTF16 array.
 * @param [in] utf16 `the utf16 data to calculate from`
 * @return `the length of the resulting UTF8 array`
 **/
fn usz utf8len_for_utf16(Char16[] utf16)
{
	usz len = 0;
	usz len16 = utf16.len;
	for (usz i = 0; i < len16; i++)
	{
		Char16 c = utf16[i];
		if (c & UTF16_SURROGATE_GENERIC_MASK != UTF16_SURROGATE_GENERIC_VALUE)
		{
			if (c <= 0x7f)
			{
				len++;
				continue;
			}
			if (c <= 0x7ff)
			{
				len += 2;
				continue;
			}
			len += 3;
			continue;
		}
		len += 4;
	}
	return len;
}

/**
 * Calculate the UTF16 length required to encode a UTF8 array.
 * @param utf8 `the utf8 data to calculate from`
 * @return `the length of the resulting UTF16 array`
 **/
fn usz utf16len_for_utf8(String utf8)
{
	usz len = utf8.len;
	usz len16 = 0;
	for (usz i = 0; i < len; i++)
	{
		len16++;
		char c = utf8[i];
		if (c & 0x80 == 0) continue;
		i++;
		if (c & 0xE0 == 0xC0) continue;
		i++;
		if (c & 0xF0 == 0xE0) continue;
		i++;
		len16++;
	}
	return len16;
}

/**
 * @param [in] utf32 `the UTF32 array to check the length for`
 * @return `the required length of an UTF16 array to hold the UTF32 data.`
 **/
fn usz utf16len_for_utf32(Char32[] utf32)
{
	usz len = utf32.len;
	foreach (Char32 uc : utf32)
	{
		if (uc >= UTF16_SURROGATE_OFFSET) len++;
	}
	return len;
}

/**
 * Convert an UTF32 array to an UTF8 array.
 *
 * @param [in] utf32
 * @param [out] utf8_buffer
 * @return `the number of bytes written.`
 **/
fn usz! utf32to8(Char32[] utf32, char[] utf8_buffer)
{
	char[] buffer = utf8_buffer;
	foreach (uc : utf32)
	{
		usz used = char32_to_utf8(uc, buffer) @inline!;
		buffer = buffer[used..];
	}
	// Zero terminate if there is space.
	if (buffer.len > 0) buffer[0] = 0;
	return utf8_buffer.len - buffer.len;
}

/**
 * Convert an UTF8 array to an UTF32 array.
 *
 * @param [in] utf8
 * @param [out] utf32_buffer
 * @return `the number of Char32s written.`
 **/
fn usz! utf8to32(String utf8, Char32[] utf32_buffer)
{
	usz len = utf8.len;
	Char32* ptr = utf32_buffer.ptr;
	usz len32 = 0;
	usz buf_len = utf32_buffer.len;
	for (usz i = 0; i < len;)
	{
		if (len32 == buf_len) return UnicodeResult.CONVERSION_FAILED?;
		usz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		i += width;
		ptr[len32++] = uc;
	}
	// Zero terminate if possible
	if (len32 + 1 < buf_len) ptr[len32] = 0;
	return len32;
}

/**
 * Copy an array of UTF16 data into an UTF8 buffer without bounds
 * checking. This will assume the buffer is sufficiently large to hold
 * the converted data.
 *
 * @param [in] utf16 `The UTF16 array containing the data to convert.`
 * @param [out] utf8_buffer `the (sufficiently large) buffer to hold the UTF16 data.`
 **/
fn void! utf16to8_unsafe(Char16[] utf16, char* utf8_buffer)
{
	usz len16 = utf16.len;
	for (usz i = 0; i < len16;)
	{
		usz available = len16 - i;
		char16_to_utf8_unsafe(&utf16[i], &available, &utf8_buffer) @inline!;
		i += available;
	}
}

/**
 * Copy an array of UTF8 data into an UTF32 buffer without bounds
 * checking. This will assume the buffer is sufficiently large to hold
 * the converted data.
 *
 * @param [in] utf8 `The UTF8 buffer containing the data to convert.`
 * @param [out] utf32_buffer `the (sufficiently large) buffer to hold the UTF8 data.`
 **/
fn void! utf8to32_unsafe(String utf8, Char32* utf32_buffer)
{
	usz len = utf8.len;
	for (usz i = 0; i < len;)
	{
		usz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		i += width;
		(utf32_buffer++)[0] = uc;
	}
}

/**
 * Copy an array of UTF8 data into an UTF16 buffer without bounds
 * checking. This will assume the buffer is sufficiently large to hold
 * the converted data.
 *
 * @param [in] utf8 `The UTF8 buffer containing the data to convert.`
 * @param [out] utf16_buffer `the (sufficiently large) buffer to hold the UTF8 data.`
 **/
fn void! utf8to16_unsafe(String utf8, Char16* utf16_buffer)
{
	usz len = utf8.len;
	for (usz i = 0; i < len;)
	{
		usz width = len - i;
		Char32 uc = utf8_to_char32(&utf8[i], &width) @inline!;
		char32_to_utf16_unsafe(uc, &utf16_buffer) @inline;
		i += width;
	}
}

/**
 * Copy an array of UTF32 code points into an UTF8 buffer without bounds
 * checking. This will assume the buffer is sufficiently large to hold
 * the converted data.
 *
 * @param [in] utf32 `The UTF32 buffer containing the data to convert.`
 * @param [out] utf8_buffer `the (sufficiently large) buffer to hold the UTF8 data.`
 **/
fn void utf32to8_unsafe(Char32[] utf32, char* utf8_buffer)
{
	char* start = utf8_buffer;
	foreach (Char32 uc : utf32)
	{
		char32_to_utf8_unsafe(uc, &utf8_buffer) @inline;
	}
}
