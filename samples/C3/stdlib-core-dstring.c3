// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/dstring.c3
// Permalink: https://github.com/c3lang/c3c/blob/4b2019c/lib/std/core/dstring.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::dstring;
import std::io;

distinct DString (OutStream) = void*;

const usz MIN_CAPACITY @private = 16;

/**
 * @require !self.data() "String already initialized"
 **/
fn DString DString.new_init(&self, usz capacity = MIN_CAPACITY, Allocator allocator = allocator::heap())
{
	if (capacity < MIN_CAPACITY) capacity = MIN_CAPACITY;
	StringData* data = allocator::alloc_with_padding(allocator, StringData, capacity)!!;
	data.allocator = allocator;
	data.len = 0;
	data.capacity = capacity;
	return *self = (DString)data;
}

/**
 * @require !self.data() "String already initialized"
 **/
fn DString DString.temp_init(&self, usz capacity = MIN_CAPACITY)
{
	self.new_init(capacity, allocator::temp()) @inline;
	return *self;
}

fn DString new_with_capacity(usz capacity, Allocator allocator = allocator::heap())
{
	return DString{}.new_init(capacity, allocator);
}

fn DString temp_with_capacity(usz capacity) => new_with_capacity(capacity, allocator::temp()) @inline;

fn DString new(String c = "", Allocator allocator = allocator::heap())
{
	usz len = c.len;
	StringData* data = (StringData*)new_with_capacity(len, allocator);
	if (len)
	{
		data.len = len;
		mem::copy(&data.chars, c.ptr, len);
	}
	return (DString)data;
}

fn DString temp_new(String s = "") => new(s, allocator::temp()) @inline;


fn void DString.replace_char(self, char ch, char replacement)
{
	StringData* data = self.data();
	foreach (&c : data.chars[:data.len])
	{
		if (*c == ch) *c = replacement;
	}
}

fn void DString.replace(&self, String needle, String replacement)
{
	StringData* data = self.data();
	usz needle_len = needle.len;
	if (!data || data.len < needle_len) return;
	usz replace_len = replacement.len;
	if (needle_len == 1 && replace_len == 1)
	{
		self.replace_char(needle[0], replacement[0]);
		return;
	}
	@pool(data.allocator) {
		String str = self.tcopy_str();
		self.clear();
		usz len = str.len;
		usz match = 0;
		foreach (i, c : str)
		{
			if (c == needle[match])
    	    {
    	        match++;
    		    if (match == needle_len)
    		    {
    			    self.append_chars(replacement);
    			    match = 0;
    			    continue;
    		    }
    		    continue;
    		}
    		if (match > 0)
    		{
    		    self.append_chars(str[i - match:match]);
    		    match = 0;
    		}
    		self.append_char(c);
    	}
    	if (match > 0) self.append_chars(str[^match:match]);
	};
}

fn DString DString.new_concat(self, DString b, Allocator allocator = allocator::heap())
{
	DString string;
	string.new_init(self.len() + b.len(), allocator);
	string.append(self);
	string.append(b);
	return string;
}

fn DString DString.temp_concat(self, DString b) => self.new_concat(b, allocator::temp());

fn ZString DString.zstr_view(&self)
{
	StringData* data = self.data();
	if (!data) return "";
	if (data.capacity == data.len)
	{
		self.reserve(1);
		data = self.data();
		data.chars[data.len] = 0;
	}
	else if (data.chars[data.len] != 0)
	{
		data.chars[data.len] = 0;
	}
	return (ZString)&data.chars[0];
}

fn usz DString.capacity(self)
{
	if (!self) return 0;
	return self.data().capacity;
}

fn usz DString.len(&self) @dynamic
{
	if (!*self) return 0;
	return self.data().len;
}

/**
 * @require new_size <= self.len()
 */
fn void DString.chop(self, usz new_size)
{
	if (!self) return;
	self.data().len = new_size;
}

fn String DString.str_view(self)
{
	StringData* data = self.data();
	if (!data) return "";
	return (String)data.chars[:data.len];
}

fn void DString.append_utf32(&self, Char32[] chars)
{
	self.reserve(chars.len);
	foreach (Char32 c : chars)
	{
		self.append_char32(c);
	}
}

/**
 * @require index < self.len()
 **/
fn void DString.set(self, usz index, char c)
{
	self.data().chars[index] = c;
}

fn void DString.append_repeat(&self, char c, usz times)
{
	if (times == 0) return;
	self.reserve(times);
	StringData* data = self.data();
	for (usz i = 0; i < times; i++)
	{
		data.chars[data.len++] = c;
	}
}

/**
 * @require c <= 0x10ffff
 */
fn void DString.append_char32(&self, Char32 c)
{
	char[4] buffer @noinit;
	char* p = &buffer;
	usz n = conv::char32_to_utf8_unsafe(c, &p);
	self.reserve(n);
	StringData* data = self.data();
	data.chars[data.len:n] = buffer[:n];
	data.len += n;
}

fn DString DString.tcopy(&self) => self.copy(allocator::temp());

fn DString DString.copy(self, Allocator allocator = null)
{
	if (!self)
	{
		if (allocator) return new_with_capacity(0, allocator);
		return (DString)null;
	}
	StringData* data = self.data();
	if (!allocator) allocator = allocator::heap();
	DString new_string = new_with_capacity(data.capacity, allocator);
	mem::copy((char*)new_string.data(), (char*)data, StringData.sizeof + data.len);
	return new_string;
}

fn ZString DString.copy_zstr(self, Allocator allocator = allocator::heap())
{
	usz str_len = self.len();
	if (!str_len)
	{
		return (ZString)allocator::calloc(allocator, 1);
	}
	char* zstr = allocator::malloc(allocator, str_len + 1);
	StringData* data = self.data();
	mem::copy(zstr, &data.chars, str_len);
	zstr[str_len] = 0;
	return (ZString)zstr;
}

fn String DString.copy_str(self, Allocator allocator = allocator::heap())
{
	return (String)self.copy_zstr(allocator)[:self.len()];
}

fn String DString.tcopy_str(self) => self.copy_str(allocator::temp()) @inline;

fn bool DString.equals(self, DString other_string)
{
	StringData *str1 = self.data();
	StringData *str2 = other_string.data();
	if (str1 == str2) return true;
	if (!str1) return str2.len == 0;
	if (!str2) return str1.len == 0;
	usz str1_len = str1.len;
	if (str1_len != str2.len) return false;
	for (int i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] != str2.chars[i]) return false;
	}
	return true;
}

fn void DString.free(&self)
{
	if (!*self) return;
	StringData* data = self.data();
	if (!data) return;
	allocator::free(data.allocator, data);
	*self = (DString)null;
}

fn bool DString.less(self, DString other_string)
{
	StringData* str1 = self.data();
	StringData* str2 = other_string.data();
	if (str1 == str2) return false;
	if (!str1) return str2.len != 0;
	if (!str2) return str1.len == 0;
	usz str1_len = str1.len;
	usz str2_len = str2.len;
	if (str1_len != str2_len) return str1_len < str2_len;
	for (int i = 0; i < str1_len; i++)
	{
		if (str1.chars[i] >= str2.chars[i]) return false;
	}
	return true;
}

fn void DString.append_chars(&self, String str)
{
	usz other_len = str.len;
	if (!other_len) return;
	if (!*self)
	{
		*self = new(str);
		return;
	}
	self.reserve(other_len);
	StringData* data = self.data();
	mem::copy(&data.chars[data.len], str.ptr, other_len);
	data.len += other_len;
}

fn Char32[] DString.copy_utf32(&self, Allocator allocator = allocator::heap())
{
	return self.str_view().to_utf32(allocator) @inline!!;
}

fn void DString.append_string(&self, DString str)
{
	StringData* other = str.data();
	if (!other) return;
	self.append(str.str_view());
}

fn void DString.clear(self)
{
	if (!self) return;
	self.data().len = 0;
}

fn usz! DString.write(&self, char[] buffer) @dynamic
{
	self.append_chars((String)buffer);
	return buffer.len;
}

fn void! DString.write_byte(&self, char c) @dynamic
{
	self.append_char(c);
}

fn void DString.append_char(&self, char c)
{
	if (!*self)
	{
		*self = new_with_capacity(MIN_CAPACITY);
	}
	self.reserve(1);
	StringData* data = self.data();
	data.chars[data.len++] = c;
}

/**
 * @require start < self.len()
 * @require end < self.len()
 * @require end >= start "End must be same or equal to the start"
 **/
fn void DString.delete_range(&self, usz start, usz end)
{
	self.delete(start, end - start + 1);
}

/**
 * @require start < self.len()
 * @require start + len <= self.len()
 **/
fn void DString.delete(&self, usz start, usz len = 1)
{
	if (!len) return;
	StringData* data = self.data();
	usz new_len = data.len - len;
	if (new_len == 0)
	{
		data.len = 0;
		return;
	}
	usz len_after = data.len - start - len;
	if (len_after > 0)
	{
		data.chars[start:len_after] = data.chars[start + len:len_after];
	}
	data.len = new_len;
}

macro void DString.append(&self, value)
{
	var $Type = $typeof(value);
	$switch ($Type)
		$case char:
		$case ichar:
			self.append_char(value);
		$case DString:
			self.append_string(value);
		$case String:
			self.append_chars(value);
		$case Char32:
			self.append_char32(value);
		$default:
			$switch
				$case $defined((Char32)value):
					self.append_char32((Char32)value);
				$case $defined((String)value):
					self.append_chars((String)value);
				$default:
					$error "Unsupported type for append – use appendf instead.";
			$endswitch
	$endswitch
}

fn void DString.insert_at(&self, usz index, String s)
{
	if (s.len == 0) return;
	self.reserve(s.len);
	StringData* data = self.data();
	usz len = self.len();
	if (data.chars[:len].ptr == s.ptr)
	{
		// Source and destination are the same: nothing to do.
		return;
	}
	index = min(index, len);
	data.len += s.len;

	char* start = data.chars[index:s.len].ptr; // area to insert into
	mem::move(start + s.len, start, len - index); // move existing data
	switch
	{
		case s.ptr <= start && start < s.ptr + s.len:
			// Overlapping areas.
			foreach_r (i, c : s)
			{
				data.chars[index + i] = c;
			}
		case start <= s.ptr && s.ptr < start + len:
			// Source has moved.
			mem::move(start, s.ptr + s.len, s.len);
		default:
			mem::move(start, s, s.len);
	}
}

fn usz! DString.appendf(&self, String format, args...) @maydiscard
{
	if (!self.data()) self.new_init(format.len + 20);
	@pool(self.data().allocator)
	{
		Formatter formatter;
		formatter.init(&out_string_append_fn, self);
		return formatter.vprintf(format, args);
	};
}

fn usz! DString.appendfn(&self, String format, args...) @maydiscard
{
	if (!self.data()) self.new_init(format.len + 20);
	@pool(self.data().allocator)
	{
		Formatter formatter;
		formatter.init(&out_string_append_fn, self);
		usz len = formatter.vprintf(format, args)!;
		self.append('\n');
		return len + 1;
	};
}

fn DString new_join(String[] s, String joiner, Allocator allocator = allocator::heap())
{
	if (!s.len) return (DString)null;
	usz total_size = joiner.len * s.len;
	foreach (String* &str : s)
	{
		total_size += str.len;
	}
	DString res = new_with_capacity(total_size, allocator);
	res.append(s[0]);
	foreach (String* &str : s[1..])
	{
		res.append(joiner);
		res.append(*str);
	}
	return res;
}

fn void! out_string_append_fn(void* data, char c) @private
{
	DString* s = data;
	s.append_char(c);
}


fn StringData* DString.data(self) @inline @private
{
	return (StringData*)self;
}

fn void DString.reserve(&self, usz addition)
{
	StringData* data = self.data();
	if (!data)
	{
		*self = dstring::new_with_capacity(addition);
		return;
	}
	usz len = data.len + addition;
	if (data.capacity >= len) return;
	usz new_capacity = data.capacity * 2;
	if (new_capacity < MIN_CAPACITY) new_capacity = MIN_CAPACITY;
	while (new_capacity < len) new_capacity *= 2;
	data.capacity = new_capacity;
	*self = (DString)allocator::realloc(data.allocator, data, StringData.sizeof + new_capacity);
}

fn usz! DString.read_from_stream(&self, InStream reader)
{
	if (&reader.available)
	{
		usz total_read = 0;
		while (usz available = reader.available()!)
		{
			self.reserve(available);
			StringData* data = self.data();
			usz len = reader.read(data.chars[data.len..(data.capacity - 1)])!;
			total_read += len;
			data.len += len;
		}
		return total_read;
	}
	usz total_read = 0;
	while (true)
	{
		// Reserve at least 16 bytes
		self.reserve(16);
		StringData* data = self.data();
		// Read into the rest of the buffer
		usz read = reader.read(data.chars[data.len..(data.capacity - 1)])!;
		data.len += read;
		// Ok, we reached the end.
		if (read < 16) return total_read;
		// Otherwise go another round
	}
}

struct StringData @private
{
	Allocator allocator;
	usz len;
	usz capacity;
	char[*] chars;
}
