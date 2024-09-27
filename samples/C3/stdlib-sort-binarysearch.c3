// Source: https://github.com/c3lang/c3c/blob/master/lib/std/sort/binarysearch.c3
// Permalink: https://github.com/c3lang/c3c/blob/fba706f/lib/std/sort/binarysearch.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::sort;

/**
 * Perform a binary search over the sorted array and return the index
 * in [0, array.len) where x would be inserted or cmp(i) is true and cmp(j) is true for j in [i, array.len).
 * @require @is_sortable(list) "The list must be sortable"
 * @require @is_valid_cmp_fn(cmp, list, context) "Expected a comparison function which compares values"
 * @require @is_valid_context(cmp, context) "Expected a valid context"
 **/
macro usz binarysearch(list, x, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	usz i;
	usz len = @len_from_list(list);
	var $no_cmp = @is_empty_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(context);
	for (usz j = len; i < j;)
	{
		usz half = i + (j - i) / 2;
		$if $no_cmp:
			switch
			{
				case greater(list[half], x): j = half;
				case less(list[half], x): i = half + 1;
				default: return half;
			}
		$else

			$switch
				$case $defined(cmp(list[0], list[0], context)):
					int res = cmp(list[half], x, context);
				$case $defined(cmp(list[0], list[0])):
					assert(!$has_context);
					int res = cmp(list[half], x);
				$case $defined(cmp(&list[0], &list[0], context)):
					int res = cmp(&list[half], &x, context);
				$case $defined(cmp(&list[0], &list[0])):
					assert(!$has_context);
					int res = cmp(&list[half], &x);
				$default:
					assert(false, "Invalid comparison function");
			$endswitch
			switch
			{
				case res > 0: j = half;
				case res < 0: i = half + 1;
				default: return half;
			}
		$endif
	}
	return i;
}