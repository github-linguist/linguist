// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/cinterop.c3
// Permalink: https://github.com/c3lang/c3c/blob/748c737/lib/std/core/cinterop.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::cinterop;

const C_INT_SIZE = $$C_INT_SIZE;
const C_LONG_SIZE = $$C_LONG_SIZE;
const C_SHORT_SIZE = $$C_SHORT_SIZE;
const C_LONG_LONG_SIZE = $$C_LONG_LONG_SIZE;

$assert C_SHORT_SIZE < 32;
$assert C_INT_SIZE < 128;
$assert C_LONG_SIZE < 128;
$assert C_LONG_LONG_SIZE <= 128;
$assert C_SHORT_SIZE <= C_INT_SIZE;
$assert C_INT_SIZE <= C_LONG_SIZE;
$assert C_LONG_SIZE <= C_LONG_LONG_SIZE;

def CShort = $typefrom(signed_int_from_bitsize($$C_SHORT_SIZE));
def CUShort = $typefrom(unsigned_int_from_bitsize($$C_SHORT_SIZE));
def CInt = $typefrom(signed_int_from_bitsize($$C_INT_SIZE));
def CUInt = $typefrom(unsigned_int_from_bitsize($$C_INT_SIZE));
def CLong = $typefrom(signed_int_from_bitsize($$C_LONG_SIZE));
def CULong = $typefrom(unsigned_int_from_bitsize($$C_LONG_SIZE));
def CLongLong = $typefrom(signed_int_from_bitsize($$C_LONG_LONG_SIZE));
def CULongLong = $typefrom(unsigned_int_from_bitsize($$C_LONG_LONG_SIZE));
def CSChar = ichar;
def CUChar = char;

def CChar = $typefrom($$C_CHAR_IS_SIGNED ? ichar.typeid : char.typeid);

// Helper macros
macro typeid signed_int_from_bitsize(usz $bitsize) @private
{
	$switch ($bitsize)
		$case 128: return int128.typeid;
		$case 64: return long.typeid;
		$case 32: return int.typeid;
		$case 16: return short.typeid;
		$case 8: return ichar.typeid;
		$default: $error("Invalid bitsize");
	$endswitch
}

macro typeid unsigned_int_from_bitsize(usz $bitsize) @private
{
	$switch ($bitsize)
		$case 128: return uint128.typeid;
		$case 64: return ulong.typeid;
		$case 32: return uint.typeid;
		$case 16: return ushort.typeid;
		$case 8: return char.typeid;
		$default: $error("Invalid bitsize");
	$endswitch
}
