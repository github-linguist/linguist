// Source: https://github.com/c3lang/c3c/blob/master/lib/std/io/io.c3
// Permalink: https://github.com/c3lang/c3c/blob/2092e21/lib/std/io/io.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2022 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::io;
import libc;

enum Seek
{
	SET,
	CURSOR,
	END
}

fault IoError
{
	ALREADY_EXISTS,
	BUSY,
	CANNOT_READ_DIR,
	DIR_NOT_EMPTY,
	EOF,
	FILE_CANNOT_DELETE,
	FILE_IS_DIR,
	FILE_IS_PIPE,
	FILE_NOT_DIR,
	FILE_NOT_FOUND,
	FILE_NOT_VALID,
	GENERAL_ERROR,
	ILLEGAL_ARGUMENT,
	INCOMPLETE_WRITE,
	INTERRUPTED,
	INVALID_POSITION,
	INVALID_PUSHBACK,
	NAME_TOO_LONG,
	NOT_SEEKABLE,
	NO_PERMISSION,
	OUT_OF_SPACE,
	OVERFLOW,
	READ_ONLY,
	SYMLINK_FAILED,
	TOO_MANY_DESCRIPTORS,
	UNEXPECTED_EOF,
	UNKNOWN_ERROR,
	UNSUPPORTED_OPERATION,
	WOULD_BLOCK,
}


/**
 * Read from a stream (default is stdin) to the next "\n"
 * or to the end of the stream, whatever comes first.
 * "\r" will be filtered from the String.
 *
 * @param stream `The stream to read from.`
 * @require @is_instream(stream) `The stream must implement InStream.`
 * @param [inout] allocator `the allocator to use.`
 * @return `The string containing the data read.`
 **/
macro String! readline(stream = io::stdin(), Allocator allocator = allocator::heap())
{
	bool $is_stream = @typeis(stream, InStream);
	$if $is_stream:
		$typeof(&stream.read_byte) func = &stream.read_byte;
		char val = func((void*)stream)!;
	$else
		char val = stream.read_byte()!;
	$endif

    if (val == '\n') return "";
    @pool(allocator)
    {
        DString str = dstring::temp_with_capacity(256);
    	if (val != '\r') str.append(val);
    	while (1)
    	{
    	    $if $is_stream:
    	        char! c = func((void*)stream);
    	    $else
    	        char! c = stream.read_byte();
    	    $endif
    		if (catch err = c)
    		{
    			if (err == IoError.EOF) break;
    			return err?;
    		}
    		if (c == '\r') continue;
    		if (c == '\n') break;
    		str.append_char(c);
    	}
    	return str.copy_str(allocator);
    };
}

/**
 * Reads a string, see `readline`, except the it is allocated
 * on the temporary allocator and does not need to be freed.
 *
 * @param stream `The stream to read from.`
 * @require @is_instream(stream) `The stream must implement InStream.`
 * @return `The temporary string containing the data read.`
 **/
macro String! treadline(stream = io::stdin())
{
	return readline(stream, allocator::temp()) @inline;
}

/**
 * Print a value to a stream.
 *
 * @param out `the stream to print to`
 * @param x `the value to print`
 * @require @is_outstream(out) `The output must implement OutStream.`
 * @return `the number of bytes printed.`
 */
macro usz! fprint(out, x)
{
	var $Type = $typeof(x);
	$switch ($Type)
		$case String: return out.write(x);
		$case ZString: return out.write(x.str_view());
		$case DString: return out.write(x.str_view());
		$default:
			$if $assignable(x, String):
				return out.write((String)x);
			$else
				return fprintf(out, "%s", x);
			$endif
	$endswitch
}

/**
 * Prints using a 'printf'-style formatting string.
 * See `printf` for details on formatting.
 *
 * @param [inout] out `The OutStream to print to`
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! fprintf(OutStream out, String format, args...)
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	return formatter.vprintf(format, args);
}

/**
 * Prints using a 'printf'-style formatting string,
 * appending '\n' at the end. See `printf`.
 *
 * @param [inout] out `The OutStream to print to`
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! fprintfn(OutStream out, String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putstream_fn, &out);
	usz len = formatter.vprintf(format, args)!;
	out.write_byte('\n')!;
	if (&out.flush) out.flush()!;
	return len + 1;
}

/**
 * @require @is_outstream(out) "The output must implement OutStream"
 */
macro usz! fprintn(out, x = "")
{
	usz len = fprint(out, x)!;
	out.write_byte('\n')!;
	$switch
		$case @typeid(out) == OutStream.typeid:
			if (&out.flush) out.flush()!;
		$case $defined(out.flush):
			out.flush()!;
	$endswitch
	return len + 1;
}

/**
 * Print any value to stdout.
 **/
macro void print(x)
{
	(void)fprint(io::stdout(), x);
}

/**
 * Print any value to stdout, appending an '\n’ after.
 *
 * @param x "The value to print"
 **/
macro void printn(x = "")
{
	(void)fprintn(io::stdout(), x);
}

/**
 * Print any value to stderr.
 **/
macro void eprint(x)
{
	(void)fprint(io::stderr(), x);
}

/**
 * Print any value to stderr, appending an '\n’ after.
 *
 * @param x "The value to print"
 **/
macro void eprintn(x)
{
	(void)fprintn(io::stderr(), x);
}


fn void! out_putstream_fn(void* data, char c) @private
{
	OutStream* stream = data;
	return (*stream).write_byte(c);
}

fn void! out_putchar_fn(void* data @unused, char c) @private
{
	libc::putchar(c);
}

/**
 * Prints using a 'printf'-style formatting string.
 * To print integer numbers, use "%d" or "%x"/"%X,
 * the latter gives the hexadecimal representation.
 *
 * All types can be printed using "%s" which gives
 * the default representation of the value.
 *
 * To create a custom output for a type, implement
 * the Printable interface.
 *
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! printf(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
	return formatter.vprintf(format, args);
}

/**
 * Prints using a 'printf'-style formatting string,
 * appending '\n' at the end. See `printf`.
 *
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! printfn(String format, args...) @maydiscard
{
	Formatter formatter;
	formatter.init(&out_putchar_fn);
	usz! len = formatter.vprintf(format, args);
	putchar('\n');
	io::stdout().flush()!;
	return len + 1;
}

/**
 * Prints using a 'printf'-style formatting string
 * to stderr.
 *
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! eprintf(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	return formatter.vprintf(format, args);
}


/**
 * Prints using a 'printf'-style formatting string,
 * to stderr appending '\n' at the end. See `printf`.
 *
 * @param [in] format `The printf-style format string`
 * @return `the number of characters printed`
 **/
fn usz! eprintfn(String format, args...) @maydiscard
{
	Formatter formatter;
	OutStream stream = stderr();
	formatter.init(&out_putstream_fn, &stream);
	usz! len = formatter.vprintf(format, args);
	stderr().write_byte('\n')!;
	stderr().flush()!;
	return len + 1;
}

/**
 * Prints using a 'printf'-style formatting string,
 * to a string buffer. See `printf`.
 *
 * @param [inout] buffer `The buffer to print to`
 * @param [in] format `The printf-style format string`
 * @return `a slice formed from the "buffer" with the resulting length.`
 **/
fn char[]! bprintf(char[] buffer, String format, args...) @maydiscard
{
	Formatter formatter;
	BufferData data = { .buffer = buffer };
	formatter.init(&out_buffer_fn, &data);
	usz size = formatter.vprintf(format, args)!;
	return buffer[:data.written];
}

// Used to print to a buffer.
fn void! out_buffer_fn(void *data, char c) @private
{
	BufferData *buffer_data = data;
	if (buffer_data.written >= buffer_data.buffer.len) return PrintFault.BUFFER_EXCEEDED?;
	buffer_data.buffer[buffer_data.written++] = c;
}

// Used for buffer printing
struct BufferData @private
{
	char[] buffer;
	usz written;
}

// Only available with LIBC
module std::io @if (env::LIBC);
import libc;

/**
 * Libc `putchar`, prints a single character to stdout.
 **/
fn void putchar(char c) @inline
{
	libc::putchar(c);
}

/**
 * Get standard out.
 *
 * @return `stdout as a File`
 **/
fn File* stdout()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stdout());
	return &file;
}

/**
 * Get standard err.
 *
 * @return `stderr as a File`
 **/
fn File* stderr()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stderr());
	return &file;
}

/**
 * Get standard in.
 *
 * @return `stdin as a File`
 **/
fn File* stdin()
{
	static File file;
	if (!file.file) file = file::from_handle(libc::stdin());
	return &file;
}

module std::io @if(!env::LIBC);

File stdin_file;
File stdout_file;
File stderr_file;

fn void putchar(char c) @inline
{
	(void)stdout_file.write_byte(c);
}

fn File* stdout()
{
	return &stdout_file;
}

fn File* stderr()
{
	return &stderr_file;
}

fn File* stdin()
{
	return &stdin_file;
}

/**
 * Wrap bytes for reading using io functions.
 **/
fn ByteReader wrap_bytes(char[] bytes)
{
	return { bytes, 0 };
}