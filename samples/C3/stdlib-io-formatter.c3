// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/formatter.c3
// Permalink: https://github.com/c3lang/c3c/blob/d635cfb/lib/std/io/formatter.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;
import std::collections::map;
import libc;

const int PRINTF_NTOA_BUFFER_SIZE = 256;

interface Printable
{
	fn String to_string(Allocator allocator) @optional;
	fn String to_new_string(Allocator allocator) @optional @deprecated("Use to_string");
	fn usz! to_format(Formatter* formatter) @optional;
}

fault PrintFault
{
	BUFFER_EXCEEDED,
	INTERNAL_BUFFER_EXCEEDED,
	INVALID_FORMAT,
	NOT_ENOUGH_ARGUMENTS,
	INVALID_ARGUMENT,
}

def OutputFn = fn void!(void* buffer, char c);
def FloatType = double;

fn usz! Formatter.printf(&self, String format, args...)
{
	return self.vprintf(format, args) @inline;
}

struct Formatter
{
	void *data;
	OutputFn out_fn;
	struct
	{
		PrintFlags flags;
		uint width;
		uint prec;
		usz idx;
		anyfault first_fault;
	}
}

bitstruct PrintFlags : uint
{
	bool zeropad;
	bool left;
	bool plus;
	bool space;
	bool hash;
	bool uppercase;
	bool precision;
}

fn void Formatter.init(&self, OutputFn out_fn, void* data = null)
{
	*self = { .data = data, .out_fn = out_fn};
}

fn usz! Formatter.out(&self, char c) @private
{
	if (catch err = self.out_fn(self.data, c))
	{
		if (self.first_fault) return self.first_fault?;
		self.first_fault = err;
		return err?;
	}
	return 1;
}

fn usz! Formatter.print_with_function(&self, Printable arg)
{
	if (&arg.to_format)
	{
		PrintFlags old = self.flags;
		uint old_width = self.width;
		uint old_prec = self.prec;
		defer
		{
			self.flags = old;
			self.width = old_width;
			self.prec = old_prec;
		}
		if (!arg) return self.out_substr("(null)");
		return arg.to_format(self);
	}
	if (&arg.to_string)
	{
		PrintFlags old = self.flags;
		uint old_width = self.width;
		uint old_prec = self.prec;
		defer
		{
			self.flags = old;
			self.width = old_width;
			self.prec = old_prec;
		}
		if (!arg) return self.out_substr("(null)");
		@stack_mem(1024; Allocator mem)
		{
			return self.out_substr(arg.to_string(mem));
		};
	}
	return SearchResult.MISSING?;
}


fn usz! Formatter.out_str(&self, any arg) @private
{
	switch (arg.type.kindof)
	{
		case TYPEID:
			return self.out_substr("typeid");
		case VOID:
			return self.out_substr("void");
		case ANYFAULT:
		case FAULT:
			return self.out_substr((*(anyfault*)arg.ptr).nameof);
		case ANY:
			return self.out_str(*(any*)arg);
		case OPTIONAL:
			unreachable();
		case SIGNED_INT:
		case UNSIGNED_INT:
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			return self.ntoa_any(arg, 10) ?? self.out_substr("<INVALID>");
		case FLOAT:
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			return self.ftoa(float_from_any(arg)) ?? self.out_substr("ERR");
		case BOOL:
			return self.out_substr(*(bool*)arg.ptr ? "true" : "false");
		default:
	}
	usz! n = self.print_with_function((Printable)arg);
	if (try n) return n;
	if (@catch(n) != SearchResult.MISSING) n!;
	switch (arg.type.kindof)
	{
		case ENUM:
			usz i = types::any_to_int(arg, usz)!!;
			assert(i < arg.type.names.len, "Illegal enum value found, numerical value was %d.", i);
			return self.out_substr(arg.type.names[i]);
		case STRUCT:
			return self.out_substr("<struct>");
		case UNION:
			return self.out_substr("<union>");
		case BITSTRUCT:
			return self.out_substr("<bitstruct>");
		case FUNC:
			return self.out_substr("<function>");
		case DISTINCT:
			if (arg.type == ZString.typeid)
			{
				return self.out_substr(*(ZString*)arg ? ((ZString*)arg).str_view() : "(null)");
			}
			if (arg.type == DString.typeid)
			{
				return self.out_substr(*(DString*)arg ? ((DString*)arg).str_view() : "(null)");
			}
			return self.out_str(arg.as_inner());
		case POINTER:
			typeid inner = arg.type.inner;
			void** pointer = arg.ptr;
			if (arg.type.inner != void.typeid)
			{
				any deref = any_make(*pointer, inner);
				n = self.print_with_function((Printable)deref);
				if (try n) return n;
				if (@catch(n) != SearchResult.MISSING) n!;
			}
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.width = 0;
			self.out_substr("0x")!;
			return self.ntoa_any(arg, 16);
		case ARRAY:
			// this is SomeType[*] so grab the "SomeType"
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			typeid inner = arg.type.inner;
			usz size = inner.sizeof;
			usz alen = arg.type.len;
			// Pretend this is a String
			void* ptr = (void*)arg.ptr;
			usz len = self.out('[')!;
			for (usz i = 0; i < alen; i++)
			{
				if (i != 0) len += self.out_substr(", ")!;
				len += self.out_str(any_make(ptr, inner))!;
				ptr += size;
			}
			len += self.out(']')!;
			return len;
		case VECTOR:
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			// this is SomeType[*] so grab the "SomeType"
			typeid inner = arg.type.inner;
			usz size = inner.sizeof;
			usz vlen = arg.type.len;
			// Pretend this is a String
			void* ptr = (void*)arg.ptr;
			usz len = self.out_substr("[<")!;
			for (usz i = 0; i < vlen; i++)
			{
				if (i != 0) len += self.out_substr(", ")!;
				len += self.out_str(any_make(ptr, inner))!;
				ptr += size;
			}
			len += self.out_substr(">]")!;
			return len;
		case SLICE:
			// this is SomeType[] so grab the "SomeType"
			typeid inner = arg.type.inner;
			if (inner == char.typeid)
			{
				return self.out_substr(*(String*)arg);
			}
			if (inner == void.typeid) inner = char.typeid;
			PrintFlags flags = self.flags;
			uint width = self.width;
			defer
			{
				self.flags = flags;
				self.width = width;
			}
			self.flags = {};
			self.width = 0;
			usz size = inner.sizeof;
			// Pretend this is a String
			String* temp = (void*)arg.ptr;
			void* ptr = (void*)temp.ptr;
			usz slen = temp.len;
			usz len = self.out('[')!;
			for (usz i = 0; i < slen; i++)
			{
				if (i != 0) len += self.out_substr(", ")!;
				len += self.out_str(any_make(ptr, inner))!;
				ptr += size;
			}
			len += self.out(']')!;
			return len;
		default:
	}
	return self.out_substr("Invalid type");
}




fn void! out_null_fn(void* data @unused, char c @unused) @private
{
}

macro usz! @report_fault(Formatter* f, $fault)
{
	(void)f.out_substr($fault);
	return PrintFault.INVALID_FORMAT?;
}

macro usz! @wrap_bad(Formatter* f, #action)
{
	usz! len = #action;
	if (catch err = len)
	{
		case PrintFault.BUFFER_EXCEEDED:
        case PrintFault.INTERNAL_BUFFER_EXCEEDED:
            return f.first_err(err)?;
        default:
            err = f.first_err(PrintFault.INVALID_ARGUMENT);
            f.out_substr("<INVALID>")!;
            return err?;
	}
	return len;
}

fn usz! Formatter.vprintf(&self, String format, any[] anys)
{
	self.first_fault = {};
	if (!self.out_fn)
	{
		// use null output function
		self.out_fn = &out_null_fn;
	}
	usz total_len;
	usz format_len = format.len;
	usz variant_index = 0;
	for (usz i = 0; i < format_len; i++)
	{
		// format specifier?  %[flags][width][.precision][length]
		char c = format[i];
		if (c != '%')
		{
			// no
			total_len += self.out(c)!;
			continue;
		}
		i++;
		if (i >= format_len) return @report_fault(self, "%ERR");
		c = format[i];
		if (c == '%')
		{
			total_len += self.out(c)!;
			continue;
		}
		// evaluate flags
		self.flags = {};
		while FLAG_EVAL: (true)
		{
			switch (c)
			{
				case '0': self.flags.zeropad = true;
				case '-': self.flags.left = true;
				case '+': self.flags.plus = true;
				case ' ': self.flags.space = true;
				case '#': self.flags.hash = true;
				default: break FLAG_EVAL;
			}
			if (++i >= format_len) return @report_fault(self, "%ERR");
			c = format[i];
		}
		// evaluate width field
		int! w = printf_parse_format_field(anys.ptr, anys.len, &variant_index, format.ptr, format.len, &i);
		if (catch w) return @report_fault(self, "%ERR");
		c = format[i];
		if (w < 0)
		{
			self.flags.left = true;
			w = -w;
		}
		self.width = w;
		// evaluate precision field
		self.prec = 0;
		if (c == '.')
		{
			self.flags.precision = true;
			if (++i >= format_len) return @report_fault(self, "<BAD FORMAT>");
			int! prec = printf_parse_format_field(anys.ptr, anys.len, &variant_index, format.ptr, format.len, &i);
			if (catch prec) return @report_fault(self, "<BAD FORMAT>");
			self.prec = prec < 0 ? 0 : prec;
			c = format[i];
		}

		// evaluate specifier
		uint base = 0;
		if (variant_index >= anys.len)
		{
			self.first_err(PrintFault.NOT_ENOUGH_ARGUMENTS);
			total_len += self.out_substr("<MISSING>")!;
			continue;
		}
		any current = anys[variant_index++];
		switch (c)
		{
			case 'd':
				base = 10;
				self.flags.hash = false;
			case 'X' :
				self.flags.uppercase = true;
				nextcase;
			case 'x' :
				base = 16;
			case 'O':
				self.flags.uppercase = true;
				nextcase;
			case 'o' :
				base = 8;
			case 'B':
				self.flags.uppercase = true;
				nextcase;
			case 'b' :
				base = 2;
			case 'A':
				self.flags.uppercase = true;
				nextcase;
			case 'a':
				total_len += @wrap_bad(self, self.atoa(float_from_any(current)))!;
				continue;
			case 'F' :
				self.flags.uppercase = true;
				nextcase;
			case 'f':
				total_len += @wrap_bad(self, self.ftoa(float_from_any(current)))!;
				continue;
			case 'E':
				self.flags.uppercase = true;
				nextcase;
			case 'e':
				total_len += @wrap_bad(self, self.etoa(float_from_any(current)))!;
				continue;
			case 'G':
				self.flags.uppercase = true;
				nextcase;
			case 'g':
				total_len += @wrap_bad(self, self.gtoa(float_from_any(current)))!;
				continue;
			case 'c':
				total_len += self.out_char(current)!;
				continue;
			case 's':
				if (self.flags.left)
				{
					usz len = self.out_str(current)!;
					total_len += len;
					total_len += self.pad(' ', self.width, len)!;
					continue;
				}
				OutputFn out_fn = self.out_fn;
				self.out_fn = (OutputFn)&out_null_fn;
				usz len = self.out_str(current)!;
				self.out_fn = out_fn;
				total_len += self.pad(' ', self.width, len)!;
				total_len += self.out_str(current)!;
				continue;
			case 'p':
				self.flags.zeropad = true;
				self.flags.hash = true;
				base = 16;
			default:
				self.first_err(PrintFault.INVALID_FORMAT);
				total_len += self.out_substr("<BAD FORMAT>")!;
				continue;
		}
		if (base != 10)
		{
			self.flags.plus = false;
			self.flags.space = false;
		}
		// ignore '0' flag when precision is given
		if (self.flags.precision) self.flags.zeropad = false;

		bool is_neg;
		total_len += @wrap_bad(self, self.ntoa(int_from_any(current, &is_neg), is_neg, base))!;
	}
	// termination
	//	out((char)0, buffer, idx < maxlen ? idx : maxlen - 1U, maxlen);

	// return written chars without terminating \0
	if (self.first_fault) return self.first_fault?;
	return total_len;
}


fn usz! Formatter.print(&self, String str)
{
	if (!self.out_fn)
	{
		// use null output function
		self.out_fn = &out_null_fn;
	}
	foreach (c : str) self.out(c)!;
	return self.idx;
}