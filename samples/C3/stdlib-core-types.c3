// Source: https://github.com/c3lang/c3c/blob/master/lib/std/core/types.c3
// Permalink: https://github.com/c3lang/c3c/blob/19d37ef/lib/std/core/types.c3
// Generated: 2024-09-15 14:16:13 UTC (UTC)
// License: MIT


module std::core::types;
import libc;


fault ConversionResult
{
	VALUE_OUT_OF_RANGE,
	VALUE_OUT_OF_UNSIGNED_RANGE,
}
/**
 * @require $Type.kindof.is_int() || $Type.kindof == TypeKind.ENUM "Argument was not an integer"
 **/
macro any_to_int(any v, $Type)
{
	typeid any_type = v.type;
	TypeKind kind = any_type.kindof;
	if (kind == TypeKind.ENUM)
	{
		any_type = any_type.inner;
		kind = any_type.kindof;
	}
	bool is_mixed_signed = $Type.kindof != any_type.kindof;
	$Type max = $Type.max;
	$Type min = $Type.min;
	switch (any_type)
	{
		case ichar:
			ichar c = *(char*)v.ptr;
			if (is_mixed_signed && c < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE?;
			return ($Type)c;
		case short:
			short s = *(short*)v.ptr;
			if (is_mixed_signed && s < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE?;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)s;
		case int:
			int i = *(int*)v.ptr;
			if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE?;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)i;
		case long:
			long l = *(long*)v.ptr;
			if (is_mixed_signed && l < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE?;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)l;
		case int128:
			int128 i = *(int128*)v.ptr;
			if (is_mixed_signed && i < 0) return ConversionResult.VALUE_OUT_OF_UNSIGNED_RANGE?;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)i;
		case char:
			char c = *(char*)v.ptr;
			if (c > max) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)c;
		case ushort:
			ushort s = *(ushort*)v.ptr;
			if (s > max || s < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)s;
		case uint:
			uint i = *(uint*)v.ptr;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)i;
		case ulong:
			ulong l = *(ulong*)v.ptr;
			if (l > max || l < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)l;
		case uint128:
			uint128 i = *(uint128*)v.ptr;
			if (i > max || i < min) return ConversionResult.VALUE_OUT_OF_RANGE?;
			return ($Type)i;
		default:
			unreachable();
	}
}

fn bool typeid.is_subtype_of(self, typeid other)
{
	while (self != void.typeid)
	{
		if (self == other) return true;
		self = self.parentof;
	}
	return false;
}

macro bool is_subtype_of($Type, $OtherType)
{
	var $typeid = $Type.typeid;
	$switch ($Type)
		$case $OtherType: return true;
		$default: return false;
	$endswitch
}
macro bool is_numerical($Type)
{
	var $kind = $Type.kindof;
	$if $kind == TypeKind.DISTINCT:
		return is_numerical($typefrom($Type.inner));
	$else
		return $kind == TypeKind.SIGNED_INT || $kind == TypeKind.UNSIGNED_INT || $kind == TypeKind.FLOAT
			|| $kind == TypeKind.VECTOR;
	$endif
}

fn bool TypeKind.is_int(kind) @inline
{
	return kind == TypeKind.SIGNED_INT || kind == TypeKind.UNSIGNED_INT;
}

macro bool is_slice_convertable($Type)
{
	$switch ($Type.kindof)
		$case SLICE:
			return true;
		$case POINTER:
			return $Type.inner.kindof == TypeKind.ARRAY;
		$default:
			return false;
	$endswitch
}

macro bool is_bool($Type) @const => $Type.kindof == TypeKind.BOOL;
macro bool is_int($Type) @const  => $Type.kindof == TypeKind.SIGNED_INT || $Type.kindof == TypeKind.UNSIGNED_INT;

/**
 * @require is_numerical($Type) "Expected a numerical type"
 **/
macro bool is_signed($Type) @const
{
	$switch (inner_kind($Type))
		$case SIGNED_INT:
		$case FLOAT:
			return true;
		$case VECTOR:
			return is_signed($typefrom($Type.inner));
		$default:
			return false;
	$endswitch
}

/**
 * @require is_numerical($Type) "Expected a numerical type"
 **/
macro bool is_unsigned($Type) @const
{
	$switch (inner_kind($Type))
		$case UNSIGNED_INT:
			return true;
		$case VECTOR:
			return is_unsigned($typefrom($Type.inner));
		$default:
			return false;
	$endswitch
}

macro bool is_indexable($Type) @const
{
	return $defined($Type{}[0]);
}

macro bool is_ref_indexable($Type) @const
{
	return $defined(&$Type{}[0]);
}

macro bool is_intlike($Type) @const
{
	$switch ($Type.kindof)
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			return true;
		$case VECTOR:
			return $Type.inner.kindof == TypeKind.SIGNED_INT || $Type.inner.kindof == TypeKind.UNSIGNED_INT;
		$default:
			return false;
	$endswitch
}

macro bool is_underlying_int($Type) @const
{
	$switch ($Type.kindof)
		$case SIGNED_INT:
		$case UNSIGNED_INT:
			return true;
		$case DISTINCT:
			return is_underlying_int($typefrom($Type.inner));
		$default:
			return false;
	$endswitch
}

macro bool is_float($Type) @const  => $Type.kindof == TypeKind.FLOAT;

macro bool is_floatlike($Type) @const
{
	$switch ($Type.kindof)
		$case FLOAT:
			return true;
		$case VECTOR:
			return $Type.inner.kindof == TypeKind.FLOAT;
		$default:
			return false;
	$endswitch
}

macro bool is_vector($Type) @const
{
	return $Type.kindof == TypeKind.VECTOR;
}

macro typeid inner_type($Type) @const
{
	$if $Type.kindof == TypeKind.DISTINCT:
		return inner_type($typefrom($Type.inner));
	$else
		return $Type.typeid;
	$endif
}

macro TypeKind inner_kind($Type) @const
{
	return inner_type($Type).kindof;
}

macro bool is_same($TypeA, $TypeB) @const
{
	return $TypeA.typeid == $TypeB.typeid;
}

macro bool @has_same(#a, #b, ...) @const
{
	var $type_a = @typeid(#a);
	$if $type_a != @typeid(#b):
		return false;
	$endif
	$for (var $i = 0; $i < $vacount; $i++)
		$if @typeid($vaexpr[$i]) != $type_a:
			return false;
		$endif
	$endfor
	return true;
}

macro bool may_load_atomic($Type) @const
{
	$switch ($Type.kindof)
	$case SIGNED_INT:
	$case UNSIGNED_INT:
	$case POINTER:
	$case FLOAT:
		return true;
	$case DISTINCT:
		return may_load_atomic($Type.inner);
	$default:
		return false;
	$endswitch
}

macro lower_to_atomic_compatible_type($Type) @const
{
	$switch ($Type.kindof)
    $case SIGNED_INT:
	  $case UNSIGNED_INT:
      return $Type.typeid;
		$case DISTINCT:
			return lower_to_atomic_compatible_type($Type.inner);
		$case FLOAT:
			$switch ($Type)
				$case float16: 
					return ushort.typeid;
				$case float: 
					return uint.typeid;
				$case double:
					return ulong.typeid;
				$case float128:
					return uint128.typeid;
				$default: 
					return void.typeid;
			$endswitch
		$default:
			return void.typeid;
  $endswitch
}

macro bool is_promotable_to_floatlike($Type) @const  => types::is_floatlike($Type) || types::is_int($Type);
macro bool is_promotable_to_float($Type) @const  => types::is_float($Type) || types::is_int($Type);

macro bool is_same_vector_type($Type1, $Type2) @const
{
	$if $Type1.kindof != TypeKind.VECTOR:
		return $Type2.kindof != TypeKind.VECTOR;
	$else
		return $Type1.inner == $Type2.inner && $Type1.len == $Type2.len;
	$endif
}

macro bool is_equatable_type($Type) @const
{
	$if $defined($Type.less) || $defined($Type.compare_to) || $defined($Type.equals):
		return true;
	$else
		return $Type.is_eq;
	$endif
}

/**
 * Checks if a type implements the copy protocol.
 **/
macro bool implements_copy($Type) @const
{
	return $defined($Type.copy) && $defined($Type.free);
}

macro bool is_equatable_value(value) @deprecated
{
	return is_equatable_type($typeof(value));
}

macro bool @equatable_value(#value) @const
{
	return is_equatable_type($typeof(#value));
}

macro bool @comparable_value(#value) @const
{
	$if $defined(#value.less) || $defined(#value.compare_to):
		return true;
	$else
		return $typeof(#value).is_ordered;
	$endif
}

macro bool is_comparable_value(value) @deprecated
{
	$if $defined(value.less) || $defined(value.compare_to):
		return true;
	$else
		return $typeof(value).is_ordered;
	$endif
}

enum TypeKind : char
{
	VOID,
	BOOL,
	SIGNED_INT,
	UNSIGNED_INT,
	FLOAT,
	TYPEID,
	ANYFAULT,
	ANY,
	ENUM,
	FAULT,
	STRUCT,
	UNION,
	BITSTRUCT,
	FUNC,
	OPTIONAL,
	ARRAY,
	SLICE,
	VECTOR,
	DISTINCT,
	POINTER,
	INTERFACE,
}

struct TypeEnum
{
	TypeKind type;
	usz elements;
}

