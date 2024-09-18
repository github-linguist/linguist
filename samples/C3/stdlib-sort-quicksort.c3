// Source: https://github.com/c3lang/c3c/blob/master/lib/std/sort/quicksort.c3
// Permalink: https://github.com/c3lang/c3c/blob/2748cf9/lib/std/sort/quicksort.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::sort;
import std::sort::qs;

/**
 * Sort list using the quick sort algorithm.
 * @require @is_sortable(list) "The list must be indexable and support .len or .len()"
 * @require @is_valid_cmp_fn(cmp, list, context) "Expected a comparison function which compares values"
 * @require @is_valid_context(cmp, context) "Expected a valid context"
 **/
macro quicksort(list, cmp = EMPTY_MACRO_SLOT, context = EMPTY_MACRO_SLOT) @builtin
{
	usz len = sort::@len_from_list(list);
	qs::qsort(<$typeof(list), $typeof(cmp), $typeof(context)>)(list, 0, (isz)len - 1, cmp, context);
}

module std::sort::qs(<Type, CmpFn, Context>);

def ElementType = $typeof(Type{}[0]);

struct StackElementItem @private
{
	isz low;
	isz high;
}

def Stack = StackElementItem[64] @private;

// Based on https://alienryderflex.com/quicksort by Darel Rex Finley, Public Domain.

fn void qsort(Type list, isz low, isz high, CmpFn cmp, Context context)
{
	var $has_cmp = @is_valid_macro_slot(cmp);
	var $has_context = @is_valid_macro_slot(context);
	var $cmp_by_value = $has_cmp &&& $assignable(list[0], $typefrom(CmpFn.params[0]));

	if (low >= 0 && high >= 0 && low < high)
	{
		Stack stack;
		stack[0].low = low;
		stack[0].high = high;
		isz i;
		isz l;
		isz h;
		while (i >= 0)
		{
		    l = stack[i].low;
		    h = stack[i].high;

		    if (l < h)
		    {
				ElementType pivot = list[l];
				while (l < h)
				{
					$switch
						$case $cmp_by_value && $has_context:
							while (cmp(list[h], pivot, context) >= 0 && l < h) h--;
		  					if (l < h) list[l++] = list[h];
							while (cmp(list[l], pivot, context) <= 0 && l < h) l++;
						$case $cmp_by_value:
							while (cmp(list[h], pivot) >= 0 && l < h) h--;
		  					if (l < h) list[l++] = list[h];
							while (cmp(list[l], pivot) <= 0 && l < h) l++;
						$case $has_cmp && $has_context:
							while (cmp(&list[h], &pivot, context) >= 0 && l < h) h--;
		  					if (l < h) list[l++] = list[h];
							while (cmp(&list[l], &pivot, context) <= 0 && l < h) l++;
						$case $has_cmp:
							while (cmp(&list[h], &pivot) >= 0 && l < h) h--;
		  					if (l < h) list[l++] = list[h];
							while (cmp(&list[l], &pivot) <= 0 && l < h) l++;
						$default:
							while (greater_eq(list[h], pivot) && l < h) h--;
		  					if (l < h) list[l++] = list[h];
							while (less_eq(list[l], pivot) && l < h) l++;
					$endswitch
					if (l < h) list[h--] = list[l];
				}
				list[l] = pivot;
				stack[i + 1].low = l + 1;
				stack[i + 1].high = stack[i].high;
				stack[i++].high = l;
				if (stack[i].high - stack[i].low > stack[i - 1].high - stack[i - 1].low)
				{
					@swap(stack[i], stack[i - 1]);
				}
		    }
		    else
		    {
				i--;
		    }
		}
	}
}
