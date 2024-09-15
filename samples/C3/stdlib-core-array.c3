// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/array.c3
// Permalink: https://github.com/c3lang/c3c/blob/e293c43/lib/std/core/array.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::array;
import std::core::array::slice;

/**
 * @param [in] array
 * @param [in] element
 * @return "the first index of the element"
 * @return! SearchResult.MISSING
 **/
macro index_of(array, element)
{
	foreach (i, &e : array)
	{
		if (*e == element) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * @require @typekind(array) == VECTOR || @typekind(array) == ARRAY
 * @require @typekind(array[0]) == VECTOR || @typekind(array[0]) == ARRAY
 **/
macro slice2d(array, x = 0, xlen = 0, y = 0, ylen = 0)
{
	if (xlen < 1) xlen = $typeof(array[0]).len + xlen;
	if (ylen < 1) ylen = $typeof(array).len + ylen;
	var $ElementType = $typeof(array[0][0]);
	return Slice2d(<$ElementType>) { ($ElementType*)&array, $typeof(array[0]).len, y, ylen, x, xlen };
}


/**
 * @param [in] array
 * @param [in] element
 * @return "the last index of the element"
 * @return! SearchResult.MISSING
 **/
macro rindex_of(array, element)
{
	foreach_r (i, &e : array)
	{
		if (*e == element) return i;
	}
	return SearchResult.MISSING?;
}

/**
 * Concatenate two arrays or slices, returning a slice containing the concatenation of them.
 *
 * @param [in] arr1
 * @param [in] arr2
 * @param [&inout] allocator "The allocator to use, default is the heap allocator"
 * @require @typekind(arr1) == SLICE || @typekind(arr1) == ARRAY
 * @require @typekind(arr2) == SLICE || @typekind(arr2) == ARRAY
 * @require @typeis(arr1[0], $typeof(arr2[0])) "Arrays must have the same type"
 * @ensure result.len == arr1.len + arr2.len
 **/
macro concat_new(arr1, arr2, Allocator allocator = allocator::heap())
{
	var $Type = $typeof(arr1[0]);
	$Type[] result = allocator::alloc_array(allocator, $Type, arr1.len + arr2.len);
	if (arr1.len > 0)
	{
		mem::copy(result.ptr, &arr1[0], arr1.len * $Type.sizeof, $Type.alignof, $Type.alignof);
	}
	if (arr2.len > 0)
	{
		mem::copy(&result[arr1.len], &arr2[0], arr2.len * $Type.sizeof, $Type.alignof, $Type.alignof);
	}
	return result;
}

/**
 * Concatenate two arrays or slices, returning a slice containing the concatenation of them,
 * allocated using the temp allocator.
 *
 * @param [in] arr1
 * @param [in] arr2
 * @require @typekind(arr1) == SLICE || @typekind(arr1) == ARRAY
 * @require @typekind(arr2) == SLICE || @typekind(arr2) == ARRAY
 * @require @typeis(arr1[0], $typeof(arr2[0])) "Arrays must have the same type"
 * @ensure result.len == arr1.len + arr2.len
 **/
macro tconcat(arr1, arr2) => concat(arr1, arr2, allocator::temp());

module std::core::array::slice(<Type>);

struct Slice2d
{
	Type* ptr;
	usz inner_len;
	usz ystart;
	usz ylen;
	usz xstart;
	usz xlen;
}

fn usz Slice2d.len(&self) @operator(len)
{
	return self.ylen;
}

fn usz Slice2d.count(&self)
{
	return self.ylen * self.xlen;
}

macro void Slice2d.@each(&self; @body(usz[<2>], Type))
{
	foreach (y, line : *self)
	{
		foreach (x, val : line)
		{
			@body({ x, y }, val);
		}
	}
}

macro void Slice2d.@each_ref(&self; @body(usz[<2>], Type*))
{
	foreach (y, line : *self)
	{
		foreach (x, &val : line)
		{
			@body({ x, y }, val);
		}
	}
}

/**
 * @require idy >= 0 && idy < self.ylen
 **/
macro Type[] Slice2d.get(self, usz idy) @operator([])
{
	return (self.ptr + self.inner_len * (idy + self.ystart))[self.xstart:self.xlen];
}

/**
 * @require y >= 0 && y < self.ylen
 * @require x >= 0 && x < self.xlen
 **/
fn Slice2d Slice2d.slice(&self, isz x = 0, isz xlen = 0, isz y = 0, isz ylen = 0)
{
	if (xlen < 1) xlen = self.xlen + xlen;
	if (ylen < 1) ylen = self.ylen + ylen;
	return { self.ptr, self.inner_len, y + self.ystart, ylen, x + self.xstart, xlen };
}
