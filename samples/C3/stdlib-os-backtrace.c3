// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/backtrace.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/os/backtrace.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::backtrace;
import std::collections::list, std::os, std::io;

fault BacktraceFault
{
	SEGMENT_NOT_FOUND,
	EXECUTABLE_PATH_NOT_FOUND,
	IMAGE_NOT_FOUND,
	NO_BACKTRACE_SYMBOLS,
	RESOLUTION_FAILED,
}

const Backtrace BACKTRACE_UNKNOWN = { 0, "", "", "", 0, null, false };

struct Backtrace (Printable)
{
	uptr offset;
	String function;
	String object_file;
	String file;
	uint line;
	Allocator allocator;
	bool is_inline;
}


fn bool Backtrace.has_file(&self)
{
	return self.file.len > 0;
}

fn bool Backtrace.is_unknown(&self)
{
	return !self.object_file.len;
}

fn usz! Backtrace.to_format(&self, Formatter* formatter) @dynamic
{
	String inline_suffix = self.is_inline ? " [inline]" : "";
	if (self.has_file())
	{
		return formatter.printf("%s (in %s) (%s:%d)%s", self.function, self.object_file, self.file, self.line, inline_suffix);
	}
	if (self.is_unknown())
	{
		return formatter.printf("??? (in unknown)%s", inline_suffix);
	}
	return formatter.printf("%s (in %s) (source unavailable)%s", self.function, self.object_file, inline_suffix);
}
fn void Backtrace.free(&self)
{
	if (!self.allocator) return;
	allocator::free(self.allocator, self.function);
	allocator::free(self.allocator, self.object_file);
	allocator::free(self.allocator, self.file);
}

fn Backtrace* Backtrace.init(&self, uptr offset, String function, String object_file, String file = "", uint line = 0, Allocator allocator)
{
	if (!allocator)
	{
		self.offset = offset;
		self.function = function;
		self.object_file = object_file;
		self.file = file;
		self.line = 0;
		self.allocator = null;
		return self;
	}
	self.offset = offset;
	self.function = function.copy(allocator);
	self.object_file = object_file.copy(allocator);
	self.file = file.copy(allocator);
	self.allocator = allocator;
	self.line = line;
	return self;
}

fn void*[] capture_current(void*[] buffer)
{
	if (!buffer.len) return buffer[:0];
	$switch
		$case env::POSIX:
			CInt len = posix::backtrace(buffer.ptr, buffer.len);
			return buffer[:len];
		$case env::WIN32:
			Win32_WORD len = win32::rtlCaptureStackBackTrace(0, buffer.len, buffer.ptr, null);
			return buffer[:len];
		$default:
			return buffer[:0];
	$endswitch
}

def BacktraceList = List(<Backtrace>);

def symbolize_backtrace = linux::symbolize_backtrace @if(env::LINUX);
def symbolize_backtrace = win32::symbolize_backtrace @if(env::WIN32);
def symbolize_backtrace = darwin::symbolize_backtrace @if(env::DARWIN);

fn BacktraceList! symbolize_backtrace(void*[] backtrace, Allocator allocator) @if(!env::NATIVE_STACKTRACE)
{
	return {};
}