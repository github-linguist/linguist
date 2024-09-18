// Source: https://github.com/c3lang/c3c/blob/master/lib/std/sort/sort.c3
// Permalink: https://github.com/c3lang/c3c/blob/05c5eae/lib/std/sort/sort.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::sort;

macro usz @len_from_list(&list)
{
	$if $defined(list.len()):
		return list.len();
	$else
		return list.len;
	$endif
}

macro bool @is_sortable(#list)
{
	$switch
		$case !$defined(#list[0]):
			return false;
		$case !$defined(#list.len):
			return false;
		$case $defined(&#list[0]) &&& !types::is_same($typeof(&#list[0]), $typeof(#list[0])*):
			return false;
		$default:
			return true;
	$endswitch
}

macro bool @is_valid_context(#cmp, #context)
{
	return @is_valid_macro_slot(#cmp) || @is_empty_macro_slot(#context);
}

macro bool @is_valid_cmp_fn(#cmp, #list, #context)
{
	var $Type = $typeof(#cmp);
	var $no_context = @is_empty_macro_slot(#context);
	$switch
		$case @is_empty_macro_slot(#cmp): return true;
	    $case $Type.kindof != FUNC ||| $Type.returns.kindof != SIGNED_INT: return false;
		$case $defined(#cmp(#list[0], #list[0], #context)): return true;
		$case $defined(#cmp(#list[0], #list[0])): return $no_context;
		$case $defined(#cmp(&#list[0], &#list[0], #context)): return true;
		$case $defined(#cmp(&#list[0], &#list[0])): return  $no_context;
		$default: return false;
	$endswitch
}

macro bool @is_cmp_key_fn(#key_fn, #list)
{
	$switch
		$case @is_empty_macro_slot(#key_fn): return true;
	    $case $typeof(#key_fn).kindof != FUNC: return false;
	    $case $typeof(#key_fn).returns.kindof != UNSIGNED_INT: return false;
		$case $defined(#key_fn(#list[0])): return true;
		$case $defined(#key_fn(&&(#list[0]))): return true;
		$default: return false;
	$endswitch
}