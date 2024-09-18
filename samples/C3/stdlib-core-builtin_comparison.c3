// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/builtin_comparison.c3
// Permalink: https://github.com/c3lang/c3c/blob/edfea63/lib/std/core/builtin_comparison.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

// Copyright (c) 2021-2024 Christoffer Lerno and contributors. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::builtin;

/**
 * @require types::@comparable_value(a) && types::@comparable_value(b)
 **/
macro less(a, b) @builtin
{
	$switch
	$case $defined(a.less):
		return a.less(b);
	$case $defined(a.compare_to):
		return a.compare_to(b) < 0;
	$default:
		return a < b;
	$endswitch
}

/**
 * @require types::@comparable_value(a) && types::@comparable_value(b)
 **/
macro less_eq(a, b) @builtin
{
	$switch
	$case $defined(a.less):
		return !b.less(a);
	$case $defined(a.compare_to):
		return a.compare_to(b) <= 0;
	$default:
		return a <= b;
	$endswitch
}

/**
 * @require types::@comparable_value(a) && types::@comparable_value(b)
 **/
macro greater(a, b) @builtin
{
	$switch
	$case $defined(a.less):
		return b.less(a);
	$case $defined(a.compare_to):
		return a.compare_to(b) > 0;
	$default:
		return a > b;
	$endswitch
}

/**
 * @require types::@comparable_value(a) && types::@comparable_value(b)
 **/
macro int compare_to(a, b) @builtin
{
	$switch
	$case $defined(a.compare_to):
		return a.compare_to(b);
	$case $defined(a.less):
		return (int)b.less(a) - (int)a.less(b);
	$default:
		return (int)(a > b) - (int)(a < b);
	$endswitch
}
/**
 * @require types::@comparable_value(a) && types::@comparable_value(b)
 **/
macro greater_eq(a, b) @builtin
{
	$switch
	$case $defined(a.less):
		return !a.less(b);
	$case $defined(a.compare_to):
		return a.compare_to(b) >= 0;
	$default:
		return a >= b;
	$endswitch
}

/**
 * @require types::@equatable_value(a) && types::@equatable_value(b) `values must be equatable`
 **/
macro bool equals(a, b) @builtin
{
	$switch
	$case $defined(a.equals, a.equals(b)):
		return a.equals(b);
	$case $defined(a.compare_to, a.compare_to(b)):
		return a.compare_to(b) == 0;
	$case $defined(a.less):
		return !a.less(b) && !b.less(a);
	$default:
		return a == b;
	$endswitch
}

macro min(x, ...) @builtin
{
	$if $vacount == 1:
		return less(x, $vaarg[0]) ? x : $vaarg[0];
	$else
		var result = x;
		$for (var $i = 0; $i < $vacount; $i++)
			if (less($vaarg[$i], result))
			{
				 result = $vaarg[$i];
			}
		$endfor
		return result;
	$endif
}

macro max(x, ...) @builtin
{
	$if $vacount == 1:
		return greater(x, $vaarg[0]) ? x : $vaarg[0];
	$else
		var result = x;
		$for (var $i = 0; $i < $vacount; $i++)
			if (greater($vaarg[$i], result))
			{
				 result = $vaarg[$i];
			}
		$endfor
		return result;
	$endif
}

