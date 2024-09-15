// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/stream.c3
// Permalink: https://github.com/c3lang/c3c/blob/2092e21/lib/std/io/stream.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::io;
import std::math;

interface InStream
{
	fn void! close() @optional;
	fn usz! seek(isz offset, Seek seek) @optional;
	fn usz len() @optional;
	fn usz! available() @optional;
	fn usz! read(char[] buffer);
	fn char! read_byte();
	fn usz! write_to(OutStream out) @optional;
	fn void! pushback_byte() @optional;
}


interface OutStream
{
	fn void! destroy() @optional;
	fn void! close() @optional;
	fn void! flush() @optional;
	fn usz! write(char[] bytes);
	fn void! write_byte(char c);
	fn usz! read_to(InStream in) @optional;
}

fn usz! available(InStream s)
{
	if (&s.available) return s.available();
	if (&s.seek)
	{
		usz curr = s.seek(0, Seek.CURSOR)!;
		usz len = s.seek(0, Seek.END)!;
		s.seek(curr, Seek.SET)!;
		return len - curr;
	}
	return 0;
}

macro bool @is_instream(#expr)
{
	return $assignable(#expr, InStream);
}

macro bool @is_outstream(#expr)
{
	return $assignable(#expr, OutStream);
}

/**
 * @param [&out] ref
 * @require @is_instream(stream)
 **/
macro usz! read_any(stream, any ref)
{
	return read_all(stream, ((char*)ref)[:ref.type.sizeof]);
}

/**
 * @param [&in] ref "the object to write."
 * @require @is_outstream(stream)
 * @ensure return == ref.type.sizeof
 */
macro usz! write_any(stream, any ref)
{
	return write_all(stream, ((char*)ref)[:ref.type.sizeof]);
}

/**
 * @require @is_instream(stream)
 */
macro usz! read_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	usz n = stream.read(buffer)!;
	if (n != buffer.len) return IoError.UNEXPECTED_EOF?;
	return n;
}

/**
 * @require @is_instream(stream)
 */
macro char[]! read_new_fully(stream, Allocator allocator = allocator::heap())
{
	usz len = available(stream)!;
	char* data = allocator::malloc_try(allocator, len)!;
	defer catch allocator::free(allocator, data);
	usz read = 0;
	while (read < len)
	{
		read += stream.read(data[read:len - read])!;
	}
	return data[:len];
}

/**
 * @require @is_outstream(stream)
 */
macro usz! write_all(stream, char[] buffer)
{
	if (buffer.len == 0) return 0;
	usz n = stream.write(buffer)!;
	if (n != buffer.len) return IoError.INCOMPLETE_WRITE?;
	return n;
}

macro usz! @read_using_read_byte(&s, char[] buffer)
{
	usz len = 0;
	foreach (&cptr : buffer)
	{
		char! c = s.read_byte();
		if (catch err = c)
		{
			case IoError.EOF: return len;
			default: return err?;
		}
		*cptr = c;
		len++;
	}
	return len;
}

macro void! @write_byte_using_write(&s, char c)
{
	char[1] buff = { c };
	(*s).write(&buff)!;
}


macro char! @read_byte_using_read(&s)
{
	char[1] buffer;
	usz read = (*s).read(&buffer)!;
	if (read != 1) return IoError.EOF?;
	return buffer[0];
}

def ReadByteFn = fn char!();


macro usz! @write_using_write_byte(&s, char[] bytes)
{
	foreach (c : bytes) s.write_byte(self, c)!;
	return bytes.len;
}

macro void! @pushback_using_seek(&s)
{
	s.seek(-1, CURSOR)!;
}

fn usz! copy_to(InStream in, OutStream dst, char[] buffer = {})
{
	if (buffer.len) return copy_through_buffer(in, dst, buffer);
	if (&in.write_to) return in.write_to(dst);
	if (&dst.read_to) return dst.read_to(in);
	$switch (env::MEMORY_ENV)
		$case NORMAL:
			return copy_through_buffer(in, dst, &&char[4096]{});
		$case SMALL:
			return copy_through_buffer(in, dst, &&char[1024]{});
		$case TINY:
		$case NONE:
			return copy_through_buffer(in, dst, &&(char[256]{}));
	$endswitch
}

macro usz! copy_through_buffer(InStream in, OutStream dst, char[] buffer) @local
{
	usz total_copied;
	while (true)
	{
		usz! len = in.read(buffer);
		if (catch err = len)
		{
			case IoError.EOF: return total_copied;
			default: return err?;
		}
		if (!len) return total_copied;
		usz written = dst.write(buffer[:len])!;
		total_copied += len;
		if (written != len) return IoError.INCOMPLETE_WRITE?;
	}
}

const char[*] MAX_VARS @private = { [2] = 3, [4] = 5, [8] = 10 };

/**
 * @require @is_instream(stream)
 * @require @typekind(x_ptr) == POINTER && $typeof(x_ptr).inner.kindof.is_int()
 **/
macro usz! read_varint(stream, x_ptr)
{
	var $Type = $typefrom($typeof(x_ptr).inner);
	const MAX = MAX_VARS[$Type.sizeof];
	$Type x;
	uint shift;
	usz n;
	for (usz i = 0; i < MAX; i++)
	{
		char! c = stream.read_byte();
		if (catch err = c)
		{
			case IoError.EOF:
				return IoError.UNEXPECTED_EOF?;
			default:
				return err?;
		}
		n++;
		if (c & 0x80 == 0)
		{
			if (i + 1 == MAX && c > 1) break;
			x |= c << shift;
			$if $Type.kindof == SIGNED_INT:
				x = x & 1 == 0 ? x >> 1 : ~(x >> 1);
			$endif
			*x_ptr = x;
			return n;
		}
		x |= (c & 0x7F) << shift;
		shift += 7;
	}
	return MathError.OVERFLOW?;
}
/**
 * @require @is_outstream(stream)
 * @require @typekind(x).is_int()
 **/
macro usz! write_varint(stream, x)
{
	var $Type = $typeof(x);
	const MAX = MAX_VARS[$Type.sizeof];
	char[MAX] buffer @noinit;
	usz i;
	while (x >= 0x80)
	{
		buffer[i] = (char)(x | 0x80);
		x >>= 7;
		i++;
	}
	buffer[i] = (char)x;
	return write_all(stream, buffer[:i + 1]);
}