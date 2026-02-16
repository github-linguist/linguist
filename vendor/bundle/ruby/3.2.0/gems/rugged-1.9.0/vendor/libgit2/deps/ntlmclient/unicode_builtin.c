/*
 * Copyright (c) Edward Thomson.  All rights reserved.
 *
 * This file is part of ntlmclient, distributed under the MIT license.
 * For full terms and copyright information, and for third-party
 * copyright information, see the included LICENSE.txt file.
 */

#include <stdlib.h>
#include <stdint.h>

#include "ntlm.h"
#include "unicode.h"
#include "compat.h"
#include "util.h"

typedef unsigned int    UTF32;   /* at least 32 bits */
typedef unsigned short  UTF16;   /* at least 16 bits */
typedef unsigned char   UTF8;    /* typically 8 bits */

/* Some fundamental constants */
#define UNI_REPLACEMENT_CHAR (UTF32)0x0000FFFD
#define UNI_MAX_BMP (UTF32)0x0000FFFF
#define UNI_MAX_UTF16 (UTF32)0x0010FFFF
#define UNI_MAX_UTF32 (UTF32)0x7FFFFFFF
#define UNI_MAX_LEGAL_UTF32 (UTF32)0x0010FFFF

#define UNI_MAX_UTF8_BYTES_PER_CODE_POINT 4

typedef enum {
	conversionOK,           /* conversion successful */
	sourceExhausted,        /* partial character in source, but hit end */
	targetExhausted,        /* insuff. room in target for conversion */
	sourceIllegal           /* source sequence is illegal/malformed */
} ConversionResult;

typedef enum {
	strictConversion = 0,
	lenientConversion
} ConversionFlags;


static const int halfShift  = 10; /* used for shifting by 10 bits */

static const UTF32 halfBase = 0x0010000UL;
static const UTF32 halfMask = 0x3FFUL;

#define UNI_SUR_HIGH_START  (UTF32)0xD800
#define UNI_SUR_HIGH_END    (UTF32)0xDBFF
#define UNI_SUR_LOW_START   (UTF32)0xDC00
#define UNI_SUR_LOW_END     (UTF32)0xDFFF
#define false       0
#define true        1

/* --------------------------------------------------------------------- */

/*
 * Index into the table below with the first byte of a UTF-8 sequence to
 * get the number of trailing bytes that are supposed to follow it.
 * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
 * left as-is for anyone who may want to do such conversion, which was
 * allowed in earlier algorithms.
 */
static const char trailingBytesForUTF8[256] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

/*
 * Magic values subtracted from a buffer value during UTF8 conversion.
 * This table contains as many values as there might be trailing bytes
 * in a UTF-8 sequence.
 */
static const UTF32 offsetsFromUTF8[6] = {
	0x00000000UL, 0x00003080UL, 0x000E2080UL,
	0x03C82080UL, 0xFA082080UL, 0x82082080UL };

/*
 * Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
 * into the first byte, depending on how many bytes follow.  There are
 * as many entries in this table as there are UTF-8 sequence types.
 * (I.e., one byte sequence, two byte... etc.). Remember that sequencs
 * for *legal* UTF-8 will be 4 or fewer bytes total.
 */
static const UTF8 firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };

/* --------------------------------------------------------------------- */

/* The interface converts a whole buffer to avoid function-call overhead.
 * Constants have been gathered. Loops & conditionals have been removed as
 * much as possible for efficiency, in favor of drop-through switches.
 * (See "Note A" at the bottom of the file for equivalent code.)
 * If your compiler supports it, the "isLegalUTF8" call can be turned
 * into an inline function.
 */

static ConversionResult ConvertUTF16toUTF8 (
        const UTF16** sourceStart, const UTF16* sourceEnd,
        UTF8** targetStart, UTF8* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF16* source = *sourceStart;
    UTF8* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch;
        unsigned short bytesToWrite = 0;
        const UTF32 byteMask = 0xBF;
        const UTF32 byteMark = 0x80;
        const UTF16* oldSource = source; /* In case we have to back up because of target overflow. */
        ch = *source++;
        /* If we have a surrogate pair, convert to UTF32 first. */
        if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
            /* If the 16 bits following the high surrogate are in the source buffer... */
            if (source < sourceEnd) {
                UTF32 ch2 = *source;
                /* If it's a low surrogate, convert to UTF32. */
                if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                    ch = ((ch - UNI_SUR_HIGH_START) << halfShift)
                        + (ch2 - UNI_SUR_LOW_START) + halfBase;
                    ++source;
                } else if (flags == strictConversion) { /* it's an unpaired high surrogate */
                    --source; /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                }
            } else { /* We don't have the 16 bits following the high surrogate. */
                --source; /* return to the high surrogate */
                result = sourceExhausted;
                break;
            }
        } else if (flags == strictConversion) {
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                --source; /* return to the illegal value itself */
                result = sourceIllegal;
                break;
            }
        }
        /* Figure out how many bytes the result will require */
        if (ch < (UTF32)0x80) {      bytesToWrite = 1;
        } else if (ch < (UTF32)0x800) {     bytesToWrite = 2;
        } else if (ch < (UTF32)0x10000) {   bytesToWrite = 3;
        } else if (ch < (UTF32)0x110000) {  bytesToWrite = 4;
        } else {                            bytesToWrite = 3;
                                            ch = UNI_REPLACEMENT_CHAR;
        }

        target += bytesToWrite;
        if (target > targetEnd) {
            source = oldSource; /* Back up source pointer! */
            target -= bytesToWrite; result = targetExhausted; break;
        }
        switch (bytesToWrite) { /* note: everything falls through. */
            case 4: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 3: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 2: *--target = (UTF8)((ch | byteMark) & byteMask); ch >>= 6;
            case 1: *--target =  (UTF8)(ch | firstByteMark[bytesToWrite]);
        }
        target += bytesToWrite;
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

/* --------------------------------------------------------------------- */

/*
 * Utility routine to tell whether a sequence of bytes is legal UTF-8.
 * This must be called with the length pre-determined by the first byte.
 * If not calling this from ConvertUTF8to*, then the length can be set by:
 *  length = trailingBytesForUTF8[*source]+1;
 * and the sequence is illegal right away if there aren't that many bytes
 * available.
 * If presented with a length > 4, this returns false.  The Unicode
 * definition of UTF-8 goes up to 4-byte sequences.
 */

NTLM_INLINE(bool) isLegalUTF8(const UTF8 *source, int length) {
    UTF8 a;
    const UTF8 *srcptr = source+length;
    switch (length) {
    default: return false;
        /* Everything else falls through when "true"... */
    case 4: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 3: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 2: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;

        switch (*source) {
            /* no fall-through in this inner switch */
            case 0xE0: if (a < 0xA0) return false; break;
            case 0xED: if (a > 0x9F) return false; break;
            case 0xF0: if (a < 0x90) return false; break;
            case 0xF4: if (a > 0x8F) return false; break;
            default:   if (a < 0x80) return false;
        }

    case 1: if (*source >= 0x80 && *source < 0xC2) return false;
    }
    if (*source > 0xF4) return false;
    return true;
}

static ConversionResult ConvertUTF8toUTF16 (
        const UTF8** sourceStart, const UTF8* sourceEnd,
        UTF16** targetStart, UTF16* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF8* source = *sourceStart;
    UTF16* target = *targetStart;
    while (source < sourceEnd) {
        UTF32 ch = 0;
        unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
        if (extraBytesToRead >= sourceEnd - source) {
            result = sourceExhausted; break;
        }
        /* Do this check whether lenient or strict */
        if (!isLegalUTF8(source, extraBytesToRead+1)) {
            result = sourceIllegal;
            break;
        }
        /*
         * The cases all fall through. See "Note A" below.
         */
        switch (extraBytesToRead) {
            case 5: ch += *source++; ch <<= 6; /* remember, illegal UTF-8 */
            case 4: ch += *source++; ch <<= 6; /* remember, illegal UTF-8 */
            case 3: ch += *source++; ch <<= 6;
            case 2: ch += *source++; ch <<= 6;
            case 1: ch += *source++; ch <<= 6;
            case 0: ch += *source++;
        }
        ch -= offsetsFromUTF8[extraBytesToRead];

        if (target >= targetEnd) {
            source -= (extraBytesToRead+1); /* Back up source pointer! */
            result = targetExhausted; break;
        }
        if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
            /* UTF-16 surrogate values are illegal in UTF-32 */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                if (flags == strictConversion) {
                    source -= (extraBytesToRead+1); /* return to the illegal value itself */
                    result = sourceIllegal;
                    break;
                } else {
                    *target++ = UNI_REPLACEMENT_CHAR;
                }
            } else {
                *target++ = (UTF16)ch; /* normal case */
            }
        } else if (ch > UNI_MAX_UTF16) {
            if (flags == strictConversion) {
                result = sourceIllegal;
                source -= (extraBytesToRead+1); /* return to the start */
                break; /* Bail out; shouldn't continue */
            } else {
                *target++ = UNI_REPLACEMENT_CHAR;
            }
        } else {
            /* target is a character in range 0xFFFF - 0x10FFFF. */
            if (target + 1 >= targetEnd) {
                source -= (extraBytesToRead+1); /* Back up source pointer! */
                result = targetExhausted; break;
            }
            ch -= halfBase;
            *target++ = (UTF16)((ch >> halfShift) + UNI_SUR_HIGH_START);
            *target++ = (UTF16)((ch & halfMask) + UNI_SUR_LOW_START);
        }
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}


bool ntlm_unicode_init(ntlm_client *ntlm)
{
	NTLM_UNUSED(ntlm);
	return true;
}

typedef enum {
	unicode_builtin_utf8_to_16,
	unicode_builtin_utf16_to_8
} unicode_builtin_encoding_direction;

NTLM_INLINE(bool) unicode_builtin_encoding_convert(
	char **converted,
	size_t *converted_len,
	ntlm_client *ntlm,
	const char *string,
	size_t string_len,
	unicode_builtin_encoding_direction direction)
{
	const char *in_start, *in_end;
	char *out, *out_start, *out_end, *new_out;
	size_t out_size, out_len;
	bool success = false;
	ConversionResult result;

	*converted = NULL;
	*converted_len = 0;

	in_start = string;
	in_end = in_start + string_len;

	/*
	 * When translating UTF8 to UTF16, these strings are only used
	 * internally, and we obey the given length, so we can simply
	 * use a buffer that is 2x the size.  Add an extra byte to NUL
	 * terminate the results (two bytes for UTF16).
	 */
	if (direction == unicode_builtin_utf8_to_16)
		out_size = (string_len * 2 + 2);
	else
		out_size = (string_len / 2 + 1);

	/* Round to the nearest multiple of 8 */
	out_size = (out_size + 7) & ~7;

	if ((out = malloc(out_size)) == NULL) {
		ntlm_client_set_errmsg(ntlm, "out of memory");
		return false;
	}

	out_start = out;
	out_end = out_start + out_size;

	/* Make room for NUL termination */
	if (direction == unicode_builtin_utf16_to_8)
		out_end--;

	while (true) {
		if (direction == unicode_builtin_utf8_to_16)
			result = ConvertUTF8toUTF16(
				(const UTF8 **)&in_start, (UTF8 *)in_end,
				(UTF16 **)&out_start, (UTF16 *)out_end, strictConversion);
		else
			result = ConvertUTF16toUTF8(
				(const UTF16 **)&in_start, (UTF16 *)in_end,
				(UTF8 **)&out_start, (UTF8 *)out_end, lenientConversion);

		switch (result) {
			case conversionOK:
				success = true;
				goto done;
			case sourceExhausted:
				ntlm_client_set_errmsg(ntlm,
					"invalid unicode string; trailing data remains");
				goto done;
			case targetExhausted:
				break;
			case sourceIllegal:
				ntlm_client_set_errmsg(ntlm,
					"invalid unicode string; trailing data remains");
				goto done;
			default:
				ntlm_client_set_errmsg(ntlm,
					"unknown unicode conversion failure");
				goto done;
		}

		/* Grow buffer size by 1.5 (rounded up to a multiple of 8) */
		out_size = ((((out_size << 1) - (out_size >> 1)) + 7) & ~7);

		if (out_size > NTLM_UNICODE_MAX_LEN) {
			ntlm_client_set_errmsg(ntlm, "unicode conversion too large");
			goto done;
		}

		out_len = out_start - out;

		if ((new_out = realloc(out, out_size)) == NULL) {
			ntlm_client_set_errmsg(ntlm, "out of memory");
			goto done;
		}

		out = new_out;
		out_start = new_out + out_len;
		out_end = out + out_size;

		/* Make room for NUL termination */
		out_end -= (direction == unicode_builtin_utf8_to_16) ? 2 : 1;
	}

done:
	if (!success) {
		free(out);
		return false;
	}

	out_len = (out_start - out);

	/* NUL terminate */
	out[out_len] = '\0';

	if (direction == unicode_builtin_utf8_to_16)
		out[out_len+1] = '\0';

	*converted = out;
	*converted_len = out_len;
	return true;
}

bool ntlm_unicode_utf8_to_16(
	char **converted,
	size_t *converted_len,
	ntlm_client *client,
	const char *string,
	size_t string_len)
{
	return unicode_builtin_encoding_convert(converted, converted_len,
		client, string, string_len, unicode_builtin_utf8_to_16);
}

bool ntlm_unicode_utf16_to_8(
	char **converted,
	size_t *converted_len,
	ntlm_client *client,
	const char *string,
	size_t string_len)
{
	return unicode_builtin_encoding_convert(converted, converted_len,
		client, string, string_len, unicode_builtin_utf16_to_8);
}

void ntlm_unicode_shutdown(ntlm_client *ntlm)
{
	NTLM_UNUSED(ntlm);
}
