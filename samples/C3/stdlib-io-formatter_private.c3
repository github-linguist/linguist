// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/formatter_private.c3
// Permalink: https://github.com/c3lang/c3c/blob/d635cfb/lib/std/io/formatter_private.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;
import std::math;

const char[16] XDIGITS_H = "0123456789ABCDEF";
const char[16] XDIGITS_L = "0123456789abcdef";

fault FormattingFault
{
	BAD_FORMAT
}

macro Formatter.first_err(&self, anyfault f)
{
	if (self.first_fault) return self.first_fault;
	self.first_fault = f;
	return f;
}

fn usz! Formatter.adjust(&self, usz len) @local
{
	if (!self.flags.left) return 0;
	return self.pad(' ', self.width, len);
}

fn uint128! int_from_any(any arg, bool *is_neg) @private
{
	switch (arg.type.kindof)
	{
		case TypeKind.POINTER:
			*is_neg = false;
			return (uint128)(uptr)*(void**)arg.ptr;
		case TypeKind.DISTINCT:
		case TypeKind.ENUM:
			return int_from_any(arg.as_inner(), is_neg);
		default:
			break;
	}
	*is_neg = false;
	switch (arg)
	{
		case bool:
			return (uint128)*arg;
		case ichar:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case short:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case int:
			int val = *arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case long:
			long val = *arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case int128:
			int128 val = *arg;
			return (*is_neg = val < 0) ? (~(uint128)val) + 1 : (uint128)val;
		case char:
			return *arg;
		case ushort:
			return *arg;
		case uint:
			return *arg;
		case ulong:
			return *arg;
		case uint128:
			return *arg;
		case float:
			float f = *arg;
			return (uint128)((*is_neg = f < 0) ? -f : f);
		case double:
			double d = *arg;
			return (uint128)((*is_neg = d < 0) ? -d : d);
		default:
			return FormattingFault.BAD_FORMAT?;
	}
}

fn FloatType! float_from_any(any arg) @private
{
	$if env::F128_SUPPORT:
		if (arg.type == float128.typeid) return (FloatType)*((float128*)arg.ptr);
	$endif
	if (arg.type.kindof == TypeKind.DISTINCT)
	{
		return float_from_any(arg.as_inner());
	}
	switch (arg)
	{
		case bool:
			return (FloatType)*arg;
		case ichar:
			return *arg;
		case short:
			return *arg;
		case int:
			return *arg;
		case long:
			return *arg;
		case int128:
			return *arg;
		case char:
			return *arg;
		case ushort:
			return *arg;
		case uint:
			return *arg;
		case ulong:
			return *arg;
		case uint128:
			return *arg;
		case float:
			return (FloatType)*arg;
		case double:
			return (FloatType)*arg;
		default:
			return FormattingFault.BAD_FORMAT?;
	}
}


/**
 * Read a simple integer value, typically for formatting.
 *
 * @param [inout] len_ptr "the length remaining."
 * @param [in] buf "the buf to read from."
 * @param maxlen "the maximum len that can be read."
 * @return "The result of the atoi."
 **/
fn uint simple_atoi(char* buf, usz maxlen, usz* len_ptr) @inline @private
{
	uint i = 0;
	usz len = *len_ptr;
	while (len < maxlen)
	{
		char c = buf[len];
		if (!c.is_digit()) break;
		i = i * 10 + c - '0';
		len++;
	}
	*len_ptr = len;
	return i;
}

fn usz! Formatter.out_substr(&self, String str) @private
{
	usz l = conv::utf8_codepoints(str);
	uint prec = self.prec;
	if (self.flags.precision && l < prec) l = prec;
	usz index = 0;
	usz chars = str.len;
	char* ptr = str.ptr;
	while (index < chars)
	{
		char c = ptr[index];
		// Break if we have precision set and we ran out...
		if (c & 0xC0 != 0x80 && self.flags.precision && !prec--) break;
		self.out(c)!;
		index++;
	}
	return index;
}

fn usz! Formatter.pad(&self, char c, isz width, isz len) @inline
{
	isz delta = width - len;
	for (isz i = 0; i < delta; i++) self.out(c)!;
	return max(0, delta);
}

fn char* fmt_u(uint128 x, char* s)
{
	for (; x > ulong.max; x /= 10) *--s = '0' + (char)(x % 10);
	for (ulong y = (ulong)x; y; y /= 10) *--s = '0' + (char)(y % 10);
	return s;
}

fn usz! Formatter.out_chars(&self, char[] s)
{
	foreach (c : s) self.out(c)!;
	return s.len;
}

enum FloatFormatting
{
	FLOAT,
	EXPONENTIAL,
	ADAPTIVE,
	HEX
}

fn usz! Formatter.etoa(&self, double y) => self.floatformat(EXPONENTIAL, y);
fn usz! Formatter.ftoa(&self, double y) => self.floatformat(FLOAT, y);
fn usz! Formatter.gtoa(&self, double y) => self.floatformat(ADAPTIVE, y);
fn usz! Formatter.atoa(&self, double y) => self.floatformat(HEX, y);

fn usz! Formatter.floatformat(&self, FloatFormatting formatting, double y) @private
{
	// This code is heavily based on musl's printf code
	const BUF_SIZE = (math::DOUBLE_MANT_DIG + 28) / 29 + 1
		+ (math::DOUBLE_MAX_EXP + math::DOUBLE_MANT_DIG + 28 + 8) / 9;
	uint[BUF_SIZE] big;
	bool is_neg = false;
	if (math::signbit(y))
	{
		is_neg = true;
		y = -y;
	}
	isz pl = is_neg || self.flags.plus ? 1 : 0;
	// Print inf/nan
	if (!math::is_finite(y))
	{
		usz len;
		// Add padding
		if (!self.flags.left) len += self.pad(' ', self.width, 3 + pl)!;
		String s = self.flags.uppercase ? "INF" : "inf";
		if (y != y) s = self.flags.uppercase ? "NAN" : "nan";
		len += s.len;
		if (pl) len += self.out(is_neg ? '-' : '+')!;
		len += self.out_chars(s)!;
		if (self.flags.left) len += self.pad(' ', self.width, 3 + pl)!;
		return len;
	}
	// Rescale
	int e2;

	y = math::frexp(y, &e2) * 2;
	if (y) e2--;
	char[12] ebuf0;
	char* ebuf = 12 + (char*)&ebuf0;
	char[9 + math::DOUBLE_MANT_DIG / 4] buf_array;
	char* buf = &buf_array;
	isz p = self.flags.precision ? self.prec : -1;
	if (formatting == HEX)
	{
		double round = 8.0;
		// 0x / 0X
		pl += 2;
		if (p > 0 && p < math::DOUBLE_MANT_DIG / 4 - 1)
		{
			int re = math::DOUBLE_MANT_DIG / 4 - 1 - (int)p;
			round *= 1 << (math::DOUBLE_MANT_DIG % 4);
			while (re--) round *= 16;
			if (is_neg)
			{
				y = -y;
				y -= round;
				y += round;
				y = -y;
			}
			else
			{
				y += round;
				y -= round;
			}
		}
		// Reverse print
		char* estr = fmt_u(e2 < 0 ? (int128)-e2 : (int128)e2, ebuf);
		if (estr == ebuf) *--estr = '0';
		*--estr = (e2 < 0 ? '-' : '+');
		*--estr = self.flags.uppercase ? 'P' : 'p';
		char* s = buf;
		char* xdigits = self.flags.uppercase ? &XDIGITS_H : &XDIGITS_L;
		do
		{
			int x = (int)y;
			*s++ = xdigits[x];
			y = 16 * (y - x);
			if (s - buf == 1 && (y || p > 0 || self.flags.hash)) *s++ = '.';
		} while (y);
		isz outlen = s - buf;
		isz explen = ebuf - estr;
		if (p > int.max - 2 - explen - pl) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
		usz len;
		usz l = p && outlen - 2 < p
				? p + 2 + explen
				: outlen + explen;
		if (!self.flags.left && !self.flags.zeropad) len += self.pad(' ', self.width, pl + l)!;
		if (is_neg || self.flags.plus) len += self.out(is_neg ? '-' : '+')!;
		len += self.out_chars(self.flags.uppercase ? "0X" : "0x")!;
		if (self.flags.zeropad) len += self.pad('0', self.width, pl + l)!;
		len += self.out_chars(buf[:outlen])!;
		len += self.pad('0', l - outlen - explen, 0)!;
		len += self.out_chars(estr[:explen])!;
		if (self.flags.left) len += self.pad(' ', self.width, pl + l)!;
		return len;
	}
	if (p < 0) p = 6;
	if (y)
	{
		y *= 0x1p28;
		e2 -= 28;
	}

	uint* a, z, r;
	if (e2 < 0)
	{
		a = r = z = &big;
	}
	else
	{
		a = r = z = (uint*)&big + big.len - math::DOUBLE_MANT_DIG - 1;
	}

	do
	{
		uint v = z++[0] = (uint)y;
		y = 1000000000 * (y - v);
	} while (y);

	while (e2 > 0)
	{
		uint carry = 0;
		int sh = math::min(29, e2);
		for (uint* d = z - 1; d >= a; d--)
		{
			ulong x = (ulong)*d << sh + carry;
			*d = (uint)(x % 1000000000);
			carry = (uint)(x / 1000000000);
		}
		if (carry) *--a = carry;
		while (z > a && !z[-1]) z--;
		e2 -= sh;
	}

	while (e2 < 0)
	{
		uint carry = 0;
		uint* b;
		int sh = math::min(9, -e2);
		int need = (int)(1 + (p + math::DOUBLE_MANT_DIG / 3u + 8) / 9);
		for (uint* d = a; d < z; d++)
		{
			// CHECK THIS
			uint rm = *d & ((1 << sh) - 1);
			*d = (*d >> sh) + carry;
			carry = (1000000000 >> sh) * rm;
		}
		if (!a[0]) a++;
		if (carry) z++[0] = carry;
		// Avoid (slow!) computation past requested precision
		b = formatting == FLOAT ? r : a;
		if (z - b > need) z = b + need;
		e2 += sh;
	}

	int e;
	if (a < z)
	{
		for (int i = 10, e = (int)(9 * (r - a)); *a >= i; i *= 10, e++);
	}

	// Perform rounding: j is precision after the radix (possibly neg)
	int j = (int)(p - (isz)(formatting == FLOAT ? 0 : e - (int)(formatting == ADAPTIVE && p)));
	if (j < 9 * (z - r - 1))
	{
		uint x;
		// We avoid C's broken division of negative numbers
		uint* d = r + 1 + ((j + 9 * math::DOUBLE_MAX_EXP) / 9 - math::DOUBLE_MAX_EXP);
		j += 9 * math::DOUBLE_MAX_EXP;
		j %= 9;
		int i;
		for (i = 10, j++; j < 9; i *= 10, j++);
		x = *d % i;
		// Are there any significant digits past j?
		if (x || (d + 1) != z)
		{
			double round = 2 / math::DOUBLE_EPSILON;
			double small;
			if (((*d / i) & 1) || (i == 1000000000 && d > a && (d[-1] & 1)))
			{
				round += 2;
			}
			switch
			{
				case x < i / 2:
					small = 0x0.8p0;
				case x == i / 2 && d + 1 == z:
					small = 0x1.0p0;
				default:
					small = 0x1.8p0;
			}
			if (pl && is_neg)
			{
				round *= -1;
				small *= -1;
			}
			*d -= x;
			// Decide whether to round by probing round+small
			if (round + small != round)
			{
				*d = *d + i;
				while (*d > 999999999)
				{
					*d-- = 0;
					if (d < a) *--a = 0;
					(*d)++;
				}
				for (i = 10, e = (int)(9 * (r - a)); *a >= i; i *= 10, e++);
			}
		}
		if (z > d + 1) z = d + 1;
	}
	for (; z>a && !z[-1]; z--);

	if (formatting == ADAPTIVE)
	{
		if (!p) p++;
		if (p > e && e >= -4)
		{
			formatting = FLOAT;
			p -= (isz)e + 1;
		}
		else
		{
			formatting = EXPONENTIAL;
			p--;
		}
		if (!self.flags.hash)
		{
			// Count trailing zeros in last place
			if (z > a && z[-1])
			{
				for (int i = 10, j = 0; z[-1] % i == 0; i *= 10, j++);
			}
			else
			{
				j = 9;
			}
			if (formatting == FLOAT)
			{
				p = math::min(p, math::max((isz)0, 9 * (z - r - 1) - j));
			}
			else
			{
				p = math::min(p, math::max((isz)0, 9 * (z - r - 1) + e - j));

			}
		}
	}
	if (p > int.max - 1 - (isz)(p || self.flags.hash)) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
	int l = (int)(1 + p + (isz)(p || self.flags.hash));
	char* estr @noinit;
	if (formatting == FLOAT)
	{
		if (e > int.max - l) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
		if (e > 0) l += e;
	}
	else
	{
		estr = fmt_u((uint128)(e < 0 ? -e : e), ebuf);
		while (ebuf - estr < 2) (--estr)[0] = '0';
		*--estr = (e < 0 ? '-' : '+');
		*--estr = self.flags.uppercase ? 'E' : 'e';
		if (ebuf - estr > (isz)int.max - l) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
		l += (int)(ebuf - estr);
	}
	if (l > int.max - pl) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
	usz len;
	if (!self.flags.left && !self.flags.zeropad) len += self.pad(' ', self.width, pl + l)!;
	if (is_neg || self.flags.plus) len += self.out(is_neg ? '-' : '+')!;
	if (self.flags.zeropad) len += self.pad('0', self.width, pl + l)!;
	if (formatting == FLOAT)
	{
		if (a > r) a = r;
		uint* d = a;
		for (; d <= r; d++)
		{
			char* s = fmt_u(*d, buf + 9);
			switch
			{
				case d != a:
					while (s > buf) (--s)[0] = '0';
				case s == buf + 9:
					*--s = '0';
			}
			len += self.out_chars(s[:buf + 9 - s])!;
		}
		if (p || self.flags.hash) len += self.out('.')!;
		for (; d < z && p > 0; d++, p -= 9)
		{
			char* s = fmt_u(*d, buf + 9);
			while (s > buf) *--s = '0';
			len += self.out_chars(s[:math::min((isz)9, p)])!;
		}
		len += self.pad('0', p + 9, 9)!;
	}
	else
	{
		if (z <= a) z = a + 1;
		for (uint* d = a; d < z && p >= 0; d++)
		{
			char* s = fmt_u(*d, buf + 9);
			if (s == buf + 9) (--s)[0] = '0';
			if (d != a)
			{
				while (s > buf) (--s)[0] = '0';
			}
			else
			{
				len += self.out(s++[0])!;
				if (p > 0 || self.flags.hash) len += self.out('.')!;
			}
			len += self.out_chars(s[:math::min(buf + 9 - s, p)])!;
			p -= buf + 9 - s;
		}
		len += self.pad('0', p + 18, 18)!;
		len += self.out_chars(estr[:ebuf - estr])!;
	}

	if (self.flags.left) len += self.pad(' ', self.width, pl + l)!;

	return len;
}

fn usz! Formatter.ntoa(&self, uint128 value, bool negative, uint base) @private
{
	char[PRINTF_NTOA_BUFFER_SIZE] buf @noinit;
	usz len;

	// no hash for 0 values
	if (!value) self.flags.hash = false;

	// write if precision != 0 or value is != 0
	if (!self.flags.precision || value)
	{
		char past_10 = (self.flags.uppercase ? 'A' : 'a') - 10;
		do
		{
			if (len >= PRINTF_NTOA_BUFFER_SIZE) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			char digit = (char)(value % base);
			buf[len++] = digit + (digit < 10 ? '0' : past_10);
			value /= base;
		}
		while (value);
	}
	return self.ntoa_format((String)buf[:PRINTF_NTOA_BUFFER_SIZE], len, negative, base);
}

fn usz! Formatter.ntoa_format(&self, String buf, usz len, bool negative, uint base) @private
{
	// pad leading zeros
	if (!self.flags.left)
	{
		if (self.width && self.flags.zeropad && (negative || self.flags.plus || self.flags.space)) self.width--;
		while (len < self.prec)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			buf[len++] = '0';
		}
		while (self.flags.zeropad && len < self.width)
		{
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			buf[len++] = '0';
		}
	}

	// handle hash
	if (self.flags.hash && base != 10)
	{
		if (!self.flags.precision && len && len == self.prec && len == self.width)
		{
			len--;
			if (len) len--;
		}
		if (base != 10)
		{
			if (len + 1 >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			switch (base)
			{
				case 16:
					buf[len++] = self.flags.uppercase ? 'X' : 'x';
				case 8:
					buf[len++] = self.flags.uppercase ? 'O' : 'o';
				case 2:
					buf[len++] = self.flags.uppercase ? 'B' : 'b';
				default:
					unreachable();
			}
			buf[len++] = '0';
		}
	}

	switch (true)
	{
		case negative:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			buf[len++] = '-';
		case self.flags.plus:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			buf[len++] = '+';
		case self.flags.space:
			if (len >= buf.len) return PrintFault.INTERNAL_BUFFER_EXCEEDED?;
			buf[len++] = ' ';
	}
	if (len) self.out_reverse(buf[:len])!;
	return len;
}


fn usz! Formatter.ntoa_any(&self, any arg, uint base) @private
{
	bool is_neg;
	return self.ntoa(int_from_any(arg, &is_neg)!!, is_neg, base) @inline;
}

fn usz! Formatter.out_char(&self, any arg) @private
{
	usz len = 1;
	uint l = 1;
	// pre padding
	len += self.adjust(l)!;
	// char output
	Char32 c = types::any_to_int(arg, uint) ?? 0xFFFD;
	switch (true)
	{
		case c < 0x7f:
			self.out((char)c)!;
		case c < 0x7ff:
			self.out((char)(0xC0 | c >> 6))!;
			self.out((char)(0x80 | (c & 0x3F)))!;
		case c < 0xffff:
			self.out((char)(0xE0 | c >> 12))!;
			self.out((char)(0x80 | (c >> 6 & 0x3F)))!;
			self.out((char)(0x80 | (c & 0x3F)))!;
		default:
			self.out((char)(0xF0 | c >> 18))!;
			self.out((char)(0x80 | (c >> 12 & 0x3F)))!;
			self.out((char)(0x80 | (c >> 6 & 0x3F)))!;
			self.out((char)(0x80 | (c & 0x3F)))!;
	}
	len += self.adjust(l)!;
	return len;
}


fn usz! Formatter.out_reverse(&self, char[] buf) @private
{
	usz n;
	usz buffer_start_idx = self.idx;
	usz len = buf.len;
	// pad spaces up to given width
	if (!self.flags.zeropad && !self.flags.left)
	{
    	n += self.pad(' ', self.width, len)!;
	}
	// reverse string
	while (len) n += self.out(buf[--len])!;

	// append pad spaces up to given width
	n += self.adjust(n)!;
	return n;
}


fn int! printf_parse_format_field(
	any* args_ptr, usz args_len, usz* args_index_ptr,
	char* format_ptr, usz format_len, usz* index_ptr) @inline @private
{
	char c = format_ptr[*index_ptr];
	if (c.is_digit()) return simple_atoi(format_ptr, format_len, index_ptr);
	if (c != '*') return 0;
	usz len = ++(*index_ptr);
    if (len >= format_len) return FormattingFault.BAD_FORMAT?;
	if (*args_index_ptr >= args_len) return FormattingFault.BAD_FORMAT?;
	any val = args_ptr[(*args_index_ptr)++];
	if (!val.type.kindof.is_int()) return FormattingFault.BAD_FORMAT?;
	uint! intval = types::any_to_int(val, int);
	return intval ?? FormattingFault.BAD_FORMAT?;
}
