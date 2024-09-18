// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/values.c3
// Permalink: https://github.com/c3lang/c3c/blob/2748cf9/lib/std/core/values.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT

module std::core::values;

macro typeid @typeid(#value) @const @builtin => $typeof(#value).typeid;
macro TypeKind @typekind(#value) @const @builtin => $typeof(#value).kindof;
macro bool @typeis(#value, $Type) @const @builtin => $typeof(#value).typeid == $Type.typeid;
/**
 * Return true if two values have the same type before any conversions.
 **/
macro bool @is_same_type(#value1, #value2) @const => $typeof(#value1).typeid == $typeof(#value2).typeid;
macro bool @is_bool(#value) @const => types::is_bool($typeof(#value));
macro bool @is_int(#value) @const => types::is_int($typeof(#value));
macro bool @is_floatlike(#value) @const => types::is_floatlike($typeof(#value));
macro bool @is_float(#value) @const => types::is_float($typeof(#value));
macro bool @is_promotable_to_floatlike(#value) @const => types::is_promotable_to_floatlike($typeof(#value));
macro bool @is_promotable_to_float(#value) @const => types::is_promotable_to_float($typeof(#value));
macro bool @is_vector(#value) @const => types::is_vector($typeof(#value));
macro bool @is_same_vector_type(#value1, #value2) @const => types::is_same_vector_type($typeof(#value1), $typeof(#value2));
macro bool @assign_to(#value1, #value2) @const => $assignable(#value1, $typeof(#value2));

macro promote_int(x)
{
	$if @is_int(x):
		return (double)x;
	$else
		return x;
	$endif
}

macro promote_int_same(x, y)
{
	$if @is_int(x):
		$switch
		$case @is_vector(y) &&& $typeof(y).inner == float.typeid:
			return (float)x;
		$case $typeof(y).typeid == float.typeid:
			return (float)x;
		$default:
			return (double)x;
		$endswitch
	$else
		return x;
	$endif
}

macro TypeKind @inner_kind(#value) @const => types::inner_kind($typeof(#value));

