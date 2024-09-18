// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/builtin.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/core/builtin.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;
import libc, std::hash, std::io, std::os::backtrace;

/**
 * Use `IteratorResult` when reading the end of an iterator, or accessing a result out of bounds.
 **/
fault IteratorResult { NO_MORE_ELEMENT }

/**
 * Use `SearchResult` when trying to return a value from some collection but the element is missing.
 **/
fault SearchResult { MISSING }

/**
 * Use `CastResult` when an attempt at conversion fails.
 **/
fault CastResult { TYPE_MISMATCH }

def VoidFn = fn void();

/**
 * Stores a variable on the stack, then restores it at the end of the
 * macro scope.
 *
 * @param variable `the variable to store and restore`
 **/
macro void @scope(&variable; @body) @builtin
{
	var temp = *variable;
	defer *variable = temp;
	@body();
}

/**
 * Swap two variables
 * @require $assignable(*b, $typeof(*a)) && $assignable(*a, $typeof(*b))
 **/
macro void @swap(&a, &b) @builtin
{
	var temp = *a;
	*a = *b;
	*b = temp;
}

/**
 * Convert an `any` type to a type, returning an failure if there is a type mismatch.
 *
 * @param v `the any to convert to the given type.`
 * @param $Type `the type to convert to`
 * @return `The any.ptr converted to its type.`
 * @ensure @typeis(return, $Type*)
 * @return! CastResult.TYPE_MISMATCH
 **/
macro anycast(any v, $Type) @builtin
{
	if (v.type != $Type.typeid) return CastResult.TYPE_MISMATCH?;
	return ($Type*)v.ptr;
}

fn bool print_backtrace(String message, int backtraces_to_ignore) @if(env::NATIVE_STACKTRACE)
{
	@pool()
	{
		void*[256] buffer;
		void*[] backtraces = backtrace::capture_current(&buffer);
		backtraces_to_ignore++;
		BacktraceList! backtrace = backtrace::symbolize_backtrace(backtraces, allocator::temp());
		if (catch backtrace) return false;
		if (backtrace.len() <= backtraces_to_ignore) return false;
		io::eprint("\nERROR: '");
        io::eprint(message);
        io::eprintn("'");
		foreach (i, &trace : backtrace)
        {
            if (i < backtraces_to_ignore) continue;
            String inline_suffix = trace.is_inline ? " [inline]" : "";
            if (trace.is_unknown())
            {
				io::eprintfn("  in ???%s", inline_suffix);
				continue;
			}
			if (trace.has_file())
            {
                io::eprintfn("  in %s (%s:%d) [%s]%s", trace.function, trace.file, trace.line, trace.object_file, inline_suffix);
                continue;
            }
            io::eprintfn("  in %s (source unavailable) [%s]%s", trace.function, trace.object_file, inline_suffix);
		}
		return true;
	};
}
fn void default_panic(String message, String file, String function, uint line) @if(env::NATIVE_STACKTRACE)
{
	$if $defined(io::stderr):
	if (!print_backtrace(message, 2))
	{
		io::eprintfn("\nERROR: '%s', in %s (%s:%d)", message, function, file, line);
		return;
	}
	$endif
	$$trap();

}

macro void abort(String string = "Unrecoverable error reached", ...) @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$$trap();
}

bool in_panic @local = false;

fn void default_panic(String message, String file, String function, uint line) @if(!env::NATIVE_STACKTRACE)
{
	if (in_panic)
	{
		io::eprintn("Panic inside of panic.");
		return;
	}
	in_panic = true;
	$if $defined(io::stderr):
	io::eprint("\nERROR: '");
	io::eprint(message);
	io::eprintfn("', in %s (%s:%d)", function, file, line);
	$endif
	in_panic = false;
	$$trap();
}

def PanicFn = fn void(String message, String file, String function, uint line);

PanicFn panic = &default_panic;

fn void panicf(String fmt, String file, String function, uint line, args...)
{
	if (in_panic)
	{
		io::eprint("Panic inside of panic: ");
		io::eprintn(fmt);
		return;
	}
	in_panic = true;
	@stack_mem(512; Allocator allocator)
	{
		DString s;
		s.new_init(allocator: allocator);
		s.appendf(fmt, ...args);
		in_panic = false;
		panic(s.str_view(), file, function, line);
	};
}

/**
 * Marks the path as unreachable. This will panic in safe mode, and in fast will simply be assumed
 * never happens.
 * @param [in] string "The panic message or format string"
 **/
macro void unreachable(String string = "Unreachable statement reached.", ...) @builtin @noreturn
{
	$if env::COMPILER_SAFE_MODE:
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$endif;
	$$unreachable();
}

/**
 * Marks the path as unsupported, this is similar to unreachable.
 * @param [in] string "The error message"
 **/
macro void unsupported(String string = "Unsupported function invoked") @builtin @noreturn
{
	panicf(string, $$FILE, $$FUNC, $$LINE, $vasplat);
	$$unreachable();
}

/**
 * Unconditionally break into an attached debugger when reached.
 **/
macro void breakpoint() @builtin
{
	$$breakpoint();
}

macro any_make(void* ptr, typeid type) @builtin
{
	return $$any_make(ptr, type);
}

macro any.retype_to(&self, typeid type)
{
	return $$any_make(self.ptr, type);
}

macro any.as_inner(&self)
{
	return $$any_make(self.ptr, self.type.inner);
}

/**
 * @param expr "the expression to cast"
 * @param $Type "the type to cast to"
 *
 * @require $sizeof(expr) == $Type.sizeof  "Cannot bitcast between types of different size."
 * @ensure @typeis(return, $Type)
 **/
macro bitcast(expr, $Type) @builtin
{
	$if $Type.alignof <= $alignof(expr):
		return *($Type*)&expr;
	$else
		$Type x @noinit;
		$$memcpy(&x, &expr, $sizeof(expr), false, $Type.alignof, $alignof(expr));
		return x;
	$endif
}

/**
 * @param $Type `The type of the enum`
 * @param [in] enum_name `The name of the enum to search for`
 * @require $Type.kindof == ENUM `Only enums may be used`
 * @ensure @typeis(return, $Type)
 * @return! SearchResult.MISSING
 **/
macro enum_by_name($Type, String enum_name) @builtin
{
	typeid x = $Type.typeid;
	foreach (i, name : x.names)
	{
		if (name == enum_name) return ($Type)i;
	}
	return SearchResult.MISSING?;
}

/**
 * Mark an expression as likely to be true
 *
 * @param #value "expression to be marked likely"
 * @param $probability "in the range 0 - 1"
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro bool @likely(bool #value, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
    $case $probability == 1.0:
	return $$expect(#value, true);
    $default:
	return $$expect_with_probability(#value, true, $probability);
    $endswitch
}

/**
 * Mark an expression as unlikely to be true
 *
 * @param #value "expression to be marked unlikely"
 * @param $probability "in the range 0 - 1"
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro bool @unlikely(bool #value, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value;
    $case $probability == 1.0:
	return $$expect(#value, false);
    $default:
	return $$expect_with_probability(#value, false, $probability);
    $endswitch
}

/**
 * @require values::@is_int(#value) || values::@is_bool(#value)
 * @require $assignable(expected, $typeof(#value))
 * @require $probability >= 0 && $probability <= 1.0
 **/
macro @expect(#value, expected, $probability = 1.0) @builtin
{
    $switch
    $case env::BUILTIN_EXPECT_IS_DISABLED:
	return #value == expected;
    $case $probability == 1.0:
	return $$expect(#value, ($typeof(#value))expected);
    $default:
	return $$expect_with_probability(#value, expected, $probability);
    $endswitch
}

/**
 * Locality for prefetch, levels 0 - 3, corresponding
 * to "extremely local" to "no locality"
 **/
enum PrefetchLocality
{
	NO_LOCALITY,
	FAR,
	NEAR,
	VERY_NEAR,
}

/**
 * Prefetch a pointer.

 * @param [in] ptr `Pointer to prefetch`
 * @param $locality `Locality ranging from none to extremely local`
 * @param $write `Prefetch for write, otherwise prefetch for read.`
 **/
macro @prefetch(void* ptr, PrefetchLocality $locality = VERY_NEAR, bool $write = false) @builtin
{
	$if !env::BUILTIN_PREFETCH_IS_DISABLED:
		$$prefetch(ptr, $write ? 1 : 0, $locality.ordinal);
	$endif
}

macro swizzle(v, ...) @builtin
{
	return $$swizzle(v, $vasplat);
}

macro swizzle2(v, v2, ...) @builtin
{
	return $$swizzle2(v, v2, $vasplat);
}

macro anyfault @catch(#expr) @builtin
{
	if (catch f = #expr) return f;
	return anyfault {};
}

macro bool @ok(#expr) @builtin
{
	if (catch #expr) return false;
	return true;
}

macro char[] @as_char_view(&value) @builtin
{
	return ((char*)value)[:$sizeof(*value)];
}

macro isz @str_find(String $string, String $needle) @builtin => $$str_find($string, $needle);
macro String @str_upper(String $str) @builtin => $$str_upper($str);
macro String @str_lower(String $str) @builtin => $$str_lower($str);
macro uint @str_hash(String $str) @builtin => $$str_hash($str);

macro uint int.hash(int i) => i;
macro uint uint.hash(uint i) => i;
macro uint short.hash(short s) => s;
macro uint ushort.hash(ushort s) => s;
macro uint char.hash(char c) => c;
macro uint ichar.hash(ichar c) => c;
macro uint long.hash(long i) => (uint)((i >> 32) ^ i);
macro uint ulong.hash(ulong i) => (uint)((i >> 32) ^ i);
macro uint int128.hash(int128 i) => (uint)((i >> 96) ^ (i >> 64) ^ (i >> 32) ^ i);
macro uint uint128.hash(uint128 i) => (uint)((i >> 96) ^ (i >> 64) ^ (i >> 32) ^ i);
macro uint bool.hash(bool b) => (uint)b;
macro uint typeid.hash(typeid t) => ((ulong)(uptr)t).hash();
macro uint String.hash(String c) => (uint)fnv32a::encode(c);
macro uint char[].hash(char[] c) => (uint)fnv32a::encode(c);
macro uint void*.hash(void* ptr) => ((ulong)(uptr)ptr).hash();

distinct EmptySlot = void*;
const EmptySlot EMPTY_MACRO_SLOT @builtin = null;
macro @is_empty_macro_slot(#arg) @builtin => @typeis(#arg, EmptySlot);
macro @is_valid_macro_slot(#arg) @builtin => !@typeis(#arg, EmptySlot);

const MAX_FRAMEADDRESS = 128;
/**
 * @require n >= 0
 **/
macro void* get_frameaddress(int n)
{
	if (n > MAX_FRAMEADDRESS) return null;
	switch (n)
	{
		case 0: return $$frameaddress(0);
		case 1: return $$frameaddress(1);
		case 2: return $$frameaddress(2);
		case 3: return $$frameaddress(3);
		case 4: return $$frameaddress(4);
		case 5: return $$frameaddress(5);
		case 6: return $$frameaddress(6);
		case 7: return $$frameaddress(7);
		case 8: return $$frameaddress(8);
		case 9: return $$frameaddress(9);
		case 10: return $$frameaddress(10);
		case 11: return $$frameaddress(11);
		case 12: return $$frameaddress(12);
		case 13: return $$frameaddress(13);
		case 14: return $$frameaddress(14);
		case 15: return $$frameaddress(15);
		case 16: return $$frameaddress(16);
		case 17: return $$frameaddress(17);
		case 18: return $$frameaddress(18);
		case 19: return $$frameaddress(19);
		case 20: return $$frameaddress(20);
		case 21: return $$frameaddress(21);
		case 22: return $$frameaddress(22);
		case 23: return $$frameaddress(23);
		case 24: return $$frameaddress(24);
		case 25: return $$frameaddress(25);
		case 26: return $$frameaddress(26);
		case 27: return $$frameaddress(27);
		case 28: return $$frameaddress(28);
		case 29: return $$frameaddress(29);
		case 30: return $$frameaddress(30);
		case 31: return $$frameaddress(31);
		case 32: return $$frameaddress(32);
		case 33: return $$frameaddress(33);
		case 34: return $$frameaddress(34);
		case 35: return $$frameaddress(35);
		case 36: return $$frameaddress(36);
		case 37: return $$frameaddress(37);
		case 38: return $$frameaddress(38);
		case 39: return $$frameaddress(39);
		case 40: return $$frameaddress(40);
		case 41: return $$frameaddress(41);
		case 42: return $$frameaddress(42);
		case 43: return $$frameaddress(43);
		case 44: return $$frameaddress(44);
		case 45: return $$frameaddress(45);
		case 46: return $$frameaddress(46);
		case 47: return $$frameaddress(47);
		case 48: return $$frameaddress(48);
		case 49: return $$frameaddress(49);
		case 50: return $$frameaddress(50);
		case 51: return $$frameaddress(51);
		case 52: return $$frameaddress(52);
		case 53: return $$frameaddress(53);
		case 54: return $$frameaddress(54);
		case 55: return $$frameaddress(55);
		case 56: return $$frameaddress(56);
		case 57: return $$frameaddress(57);
		case 58: return $$frameaddress(58);
		case 59: return $$frameaddress(59);
		case 60: return $$frameaddress(60);
		case 61: return $$frameaddress(61);
		case 62: return $$frameaddress(62);
		case 63: return $$frameaddress(63);
		case 64: return $$frameaddress(64);
		case 65: return $$frameaddress(65);
		case 66: return $$frameaddress(66);
		case 67: return $$frameaddress(67);
		case 68: return $$frameaddress(68);
		case 69: return $$frameaddress(69);
		case 70: return $$frameaddress(70);
		case 71: return $$frameaddress(71);
		case 72: return $$frameaddress(72);
		case 73: return $$frameaddress(73);
		case 74: return $$frameaddress(74);
		case 75: return $$frameaddress(75);
		case 76: return $$frameaddress(76);
		case 77: return $$frameaddress(77);
		case 78: return $$frameaddress(78);
		case 79: return $$frameaddress(79);
		case 80: return $$frameaddress(80);
		case 81: return $$frameaddress(81);
		case 82: return $$frameaddress(82);
		case 83: return $$frameaddress(83);
		case 84: return $$frameaddress(84);
		case 85: return $$frameaddress(85);
		case 86: return $$frameaddress(86);
		case 87: return $$frameaddress(87);
		case 88: return $$frameaddress(88);
		case 89: return $$frameaddress(89);
		case 90: return $$frameaddress(90);
		case 91: return $$frameaddress(91);
		case 92: return $$frameaddress(92);
		case 93: return $$frameaddress(93);
		case 94: return $$frameaddress(94);
		case 95: return $$frameaddress(95);
		case 96: return $$frameaddress(96);
		case 97: return $$frameaddress(97);
		case 98: return $$frameaddress(98);
		case 99: return $$frameaddress(99);
		case 100: return $$frameaddress(100);
		case 101: return $$frameaddress(101);
		case 102: return $$frameaddress(102);
		case 103: return $$frameaddress(103);
		case 104: return $$frameaddress(104);
		case 105: return $$frameaddress(105);
		case 106: return $$frameaddress(106);
		case 107: return $$frameaddress(107);
		case 108: return $$frameaddress(108);
		case 109: return $$frameaddress(109);
		case 110: return $$frameaddress(110);
		case 111: return $$frameaddress(111);
		case 112: return $$frameaddress(112);
		case 113: return $$frameaddress(113);
		case 114: return $$frameaddress(114);
		case 115: return $$frameaddress(115);
		case 116: return $$frameaddress(116);
		case 117: return $$frameaddress(117);
		case 118: return $$frameaddress(118);
		case 119: return $$frameaddress(119);
		case 120: return $$frameaddress(120);
		case 121: return $$frameaddress(121);
		case 122: return $$frameaddress(122);
		case 123: return $$frameaddress(123);
		case 124: return $$frameaddress(124);
		case 125: return $$frameaddress(125);
		case 126: return $$frameaddress(126);
		case 127: return $$frameaddress(127);
		case 128: return $$frameaddress(128);
		default: unreachable();
	}
}

/**
 * @require n >= 0
 **/
macro void* get_returnaddress(int n)
{
	if (n > MAX_FRAMEADDRESS) return null;
	switch (n)
	{
		case 0: return $$returnaddress(0);
		case 1: return $$returnaddress(1);
		case 2: return $$returnaddress(2);
		case 3: return $$returnaddress(3);
		case 4: return $$returnaddress(4);
		case 5: return $$returnaddress(5);
		case 6: return $$returnaddress(6);
		case 7: return $$returnaddress(7);
		case 8: return $$returnaddress(8);
		case 9: return $$returnaddress(9);
		case 10: return $$returnaddress(10);
		case 11: return $$returnaddress(11);
		case 12: return $$returnaddress(12);
		case 13: return $$returnaddress(13);
		case 14: return $$returnaddress(14);
		case 15: return $$returnaddress(15);
		case 16: return $$returnaddress(16);
		case 17: return $$returnaddress(17);
		case 18: return $$returnaddress(18);
		case 19: return $$returnaddress(19);
		case 20: return $$returnaddress(20);
		case 21: return $$returnaddress(21);
		case 22: return $$returnaddress(22);
		case 23: return $$returnaddress(23);
		case 24: return $$returnaddress(24);
		case 25: return $$returnaddress(25);
		case 26: return $$returnaddress(26);
		case 27: return $$returnaddress(27);
		case 28: return $$returnaddress(28);
		case 29: return $$returnaddress(29);
		case 30: return $$returnaddress(30);
		case 31: return $$returnaddress(31);
		case 32: return $$returnaddress(32);
		case 33: return $$returnaddress(33);
		case 34: return $$returnaddress(34);
		case 35: return $$returnaddress(35);
		case 36: return $$returnaddress(36);
		case 37: return $$returnaddress(37);
		case 38: return $$returnaddress(38);
		case 39: return $$returnaddress(39);
		case 40: return $$returnaddress(40);
		case 41: return $$returnaddress(41);
		case 42: return $$returnaddress(42);
		case 43: return $$returnaddress(43);
		case 44: return $$returnaddress(44);
		case 45: return $$returnaddress(45);
		case 46: return $$returnaddress(46);
		case 47: return $$returnaddress(47);
		case 48: return $$returnaddress(48);
		case 49: return $$returnaddress(49);
		case 50: return $$returnaddress(50);
		case 51: return $$returnaddress(51);
		case 52: return $$returnaddress(52);
		case 53: return $$returnaddress(53);
		case 54: return $$returnaddress(54);
		case 55: return $$returnaddress(55);
		case 56: return $$returnaddress(56);
		case 57: return $$returnaddress(57);
		case 58: return $$returnaddress(58);
		case 59: return $$returnaddress(59);
		case 60: return $$returnaddress(60);
		case 61: return $$returnaddress(61);
		case 62: return $$returnaddress(62);
		case 63: return $$returnaddress(63);
		case 64: return $$returnaddress(64);
		case 65: return $$returnaddress(65);
		case 66: return $$returnaddress(66);
		case 67: return $$returnaddress(67);
		case 68: return $$returnaddress(68);
		case 69: return $$returnaddress(69);
		case 70: return $$returnaddress(70);
		case 71: return $$returnaddress(71);
		case 72: return $$returnaddress(72);
		case 73: return $$returnaddress(73);
		case 74: return $$returnaddress(74);
		case 75: return $$returnaddress(75);
		case 76: return $$returnaddress(76);
		case 77: return $$returnaddress(77);
		case 78: return $$returnaddress(78);
		case 79: return $$returnaddress(79);
		case 80: return $$returnaddress(80);
		case 81: return $$returnaddress(81);
		case 82: return $$returnaddress(82);
		case 83: return $$returnaddress(83);
		case 84: return $$returnaddress(84);
		case 85: return $$returnaddress(85);
		case 86: return $$returnaddress(86);
		case 87: return $$returnaddress(87);
		case 88: return $$returnaddress(88);
		case 89: return $$returnaddress(89);
		case 90: return $$returnaddress(90);
		case 91: return $$returnaddress(91);
		case 92: return $$returnaddress(92);
		case 93: return $$returnaddress(93);
		case 94: return $$returnaddress(94);
		case 95: return $$returnaddress(95);
		case 96: return $$returnaddress(96);
		case 97: return $$returnaddress(97);
		case 98: return $$returnaddress(98);
		case 99: return $$returnaddress(99);
		case 100: return $$returnaddress(100);
		case 101: return $$returnaddress(101);
		case 102: return $$returnaddress(102);
		case 103: return $$returnaddress(103);
		case 104: return $$returnaddress(104);
		case 105: return $$returnaddress(105);
		case 106: return $$returnaddress(106);
		case 107: return $$returnaddress(107);
		case 108: return $$returnaddress(108);
		case 109: return $$returnaddress(109);
		case 110: return $$returnaddress(110);
		case 111: return $$returnaddress(111);
		case 112: return $$returnaddress(112);
		case 113: return $$returnaddress(113);
		case 114: return $$returnaddress(114);
		case 115: return $$returnaddress(115);
		case 116: return $$returnaddress(116);
		case 117: return $$returnaddress(117);
		case 118: return $$returnaddress(118);
		case 119: return $$returnaddress(119);
		case 120: return $$returnaddress(120);
		case 121: return $$returnaddress(121);
		case 122: return $$returnaddress(122);
		case 123: return $$returnaddress(123);
		case 124: return $$returnaddress(124);
		case 125: return $$returnaddress(125);
		case 126: return $$returnaddress(126);
		case 127: return $$returnaddress(127);
		case 128: return $$returnaddress(128);
		default: unreachable();
	}
}

module std::core::builtin @if((env::LINUX || env::DARWIN) && env::COMPILER_SAFE_MODE && env::DEBUG_SYMBOLS);
import libc, std::io;

fn void sig_panic(String message)
{
	default_panic(message, "???", "???", 0);
}

SignalFunction old_bus_error;
SignalFunction old_segmentation_fault;

fn void sig_bus_error(CInt i)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Illegal memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Illegal memory access.", 1))
			{
				io::eprintn("\nERROR: 'Illegal memory access'.");
			}
		$endif
	$endif
	$$trap();
}

fn void sig_segmentation_fault(CInt i)
{
	$if !env::NATIVE_STACKTRACE:
		sig_panic("Out of bounds memory access.");
	$else
		$if $defined(io::stderr):
			if (!print_backtrace("Out of bounds memory access.", 1))
			{
				io::eprintn("\nERROR: Memory error without backtrace, possible stack overflow.");
			}
		$endif
	$endif
	$$trap();
}

fn void install_signal_handler(CInt signal, SignalFunction func) @local
{
	SignalFunction old = libc::signal(signal, func);
	// Restore
	if ((iptr)old > 1024) libc::signal(signal, old);
}

// Clean this up
fn void install_signal_handlers() @init(101) @local @if(env::BACKTRACE)
{
	install_signal_handler(libc::SIGBUS, &sig_bus_error);
	install_signal_handler(libc::SIGSEGV, &sig_segmentation_fault);
}

