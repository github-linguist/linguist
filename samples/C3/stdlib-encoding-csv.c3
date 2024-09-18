// Source: https://github.com/c3lang/c3c/blob/master/lib/std/encoding/csv.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/encoding/csv.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::encoding::csv;
import std::io;

struct CsvReader
{
	InStream stream;
	String separator;
}

fn void CsvReader.init(&self, InStream stream, String separator = ",")
{
	self.stream = stream;
	self.separator = separator;
}

fn String[]! CsvReader.read_new_row(self, Allocator allocator = allocator::heap())
{
	return self.read_new_row_with_allocator(allocator::temp()) @inline;
}

fn String[]! CsvReader.read_new_row_with_allocator(self, Allocator allocator = allocator::heap())
{
	@pool(allocator)
	{
		return io::treadline(self.stream).split(self.separator, allocator: allocator);
	};
}

fn String[]! CsvReader.read_temp_row(self)
{
	return self.read_new_row_with_allocator(allocator::temp()) @inline;
}

fn void! CsvReader.skip_row(self) @maydiscard
{
	@pool()
	{
		(void)io::treadline(self.stream);
	};
}

macro CsvReader.@each_row(self, int rows = int.max; @body(String[] row))
{
	InputStream* stream = self.stream;
	String sep = self.separator;
	while (rows--)
	{
		@stack_mem(512; Allocator mem)
		{
			String[] parts;
			@pool()
			{
				String! s = stream.treadline();
				if (catch err = s)
				{
					if (err == IoError.EOF) return;
					return err?;
				}
				parts = s.split(sep, allocator: mem);
			};
			@body(parts);
		};
	}
}