// Source: https://github.com/c3lang/c3c/blob/master/lib/std/math/math.c3
// Permalink: https://github.com/c3lang/c3c/blob/edfea63/lib/std/math/math.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::math;
import std::math::complex;
import std::math::matrix;
import std::math::quaternion;

// TODO Define these using quad precision.
const E = 2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466;
const LOG2E = 1.44269504088896340735992468100189214; // log2(e)
const LOG10E = 0.434294481903251827651128918916605082; // log10(e)
const LN2 = 0.693147180559945309417232121458176568; // ln(2)
const LN10 = 2.30258509299404568401799145468436421; // ln(10)
const PI = 3.14159265358979323846264338327950288419716939937510; // pi
const PI_2 = 1.57079632679489661923132169163975144; // pi / 2
const PI_4 = 0.785398163397448309615660845819875721; // pi / 4
const DIV_PI = 0.318309886183790671537767526745028724; // 1 / pi
const DIV_2_PI = 0.636619772367581343075535053490057448; // 2 / pi
const DIV_2_SQRTPI = 1.12837916709551257389615890312154517;   // 2/sqrt(pi)
const SQRT2 = 1.41421356237309504880168872420969808; // sqrt(2)
const double DIV_1_SQRT2 = 0.707106781186547524400844362104849039; // 1 / sqrt(2)

const HALF_MAX = 6.5504e+4;
const HALF_MIN = 6.103515625e-5;
const HALF_DENORM_MIN = 5.9604644775390625e-8;
const HALF_DIG = 3;
const HALF_DEC_DIGITS = 5;
const HALF_MANT_DIG = 11;
const HALF_MAX_10_EXP = 4;
const HALF_MIN_10_EXP = -4;
const HALF_MAX_EXP = 16;
const HALF_MIN_EXP = -13;
const HALF_EPSILON = 9.765625e-4;

const FLOAT_MAX = 0x1.fffffep+127;
const FLOAT_MIN = 1.17549435e-38;
const FLOAT_DENORM_MIN = 1.40129846432481707092e-45;
const FLOAT_DIG = 6;
const FLOAT_DEC_DIGITS = 9;
const FLOAT_MANT_DIG = 24;
const FLOAT_MAX_10_EXP = 38;
const FLOAT_MIN_10_EXP = -37;
const FLOAT_MAX_EXP = 128;
const FLOAT_MIN_EXP = -125;
const FLOAT_EPSILON = 1.1920928955078125e-07;

const DOUBLE_MAX = 1.79769313486231570815e+308;
const DOUBLE_MIN = 2.2250738585072014e-308;
const DOUBLE_DENORM_MIN = 4.94065645841246544177e-324;
const DOUBLE_DIG = 15;
const DOUBLE_DEC_DIGITS = 17;
const DOUBLE_MANT_DIG = 53;
const DOUBLE_MAX_10_EXP = 308;
const DOUBLE_MIN_10_EXP = -307;
const DOUBLE_MAX_EXP = 1024;
const DOUBLE_MIN_EXP = -1021;
const DOUBLE_EPSILON = 2.22044604925031308085e-16;

const QUAD_MANT_DIG = 113;

/*
const QUAD_MAX = 1.18973149535723176508575932662800702e+4932;
const QUAD_MIN = 3.36210314311209350626267781732175260e-4932;
const QUAD_DENORM_MIN = 6.47517511943802511092443895822764655e-4966;
const QUAD_DIG = 33;
const QUAD_DEC_DIGITS = 36;
const QUAD_MAX_10_EXP = 4932;
const QUAD_MIN_10_EXP = -4931;
const QUAD_MAX_EXP = 16384;
const QUAD_MIN_EXP = -16481;
const QUAD_EPSILON = 1.92592994438723585305597794258492732e-34;
*/

enum RoundingMode : int
{
	TOWARD_ZERO,
	TO_NEAREST,
	TOWARD_INFINITY,
	TOWARD_NEG_INFINITY
}

fault MathError
{
	OVERFLOW,
}

fault MatrixError
{
	MATRIX_INVERSE_DOESNT_EXIST,
}

def Complexf = Complex(<float>);
def Complex = Complex(<double>);
def COMPLEX_IDENTITY = complex::IDENTITY(<double>);
def COMPLEXF_IDENTITY = complex::IDENTITY(<float>);

def Quaternionf = Quaternion(<float>);
def Quaternion = Quaternion(<double>);
def QUATERNION_IDENTITY = quaternion::IDENTITY(<double>);
def QUATERNIONF_IDENTITY = quaternion::IDENTITY(<float>);

def Matrix2f = Matrix2x2(<float>);
def Matrix2 = Matrix2x2(<double>);
def Matrix3f = Matrix3x3(<float>);
def Matrix3 = Matrix3x3(<double>);
def Matrix4f = Matrix4x4(<float>);
def Matrix4 = Matrix4x4(<double>);
def matrix4_ortho = matrix::ortho(<double>);
def matrix4_perspective = matrix::perspective(<double>);
def matrix4f_ortho = matrix::ortho(<float>);
def matrix4f_perspective = matrix::perspective(<float>);

def MATRIX2_IDENTITY = matrix::IDENTITY2(<double>);
def MATRIX2F_IDENTITY = matrix::IDENTITY2(<float>);
def MATRIX3_IDENTITY = matrix::IDENTITY3(<double>);
def MATRIX3F_IDENTITY = matrix::IDENTITY3(<float>);
def MATRIX4_IDENTITY = matrix::IDENTITY4(<double>);
def MATRIX4F_IDENTITY = matrix::IDENTITY4(<float>);


/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 **/
macro deg_to_rad(x) {
    return x * PI / 180;
}

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 **/
macro abs(x) => $$abs(x);

/**
 * @require values::@is_int(x) `The input must be an integer`
 **/
macro sign(x)
{
	var $Type = $typeof(x);
	$if $Type.kindof == TypeKind.UNSIGNED_INT:
		return ($Type)(x > 0);
	$else
		return ($Type)(x > 0) - ($Type)(x < 0);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 * @require values::@is_int(y) || values::@is_float(y) "Expected an integer or floating point value"
 **/
macro atan2(x, y)
{
	$if @typeis(x, float) && @typeis(y, float):
		return _atan2f(x, y);
	$else
		return _atan2(x, y);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 * @require (@typekind(y) == ARRAY || @typekind(y) == VECTOR) &&& y.len == 2
 * @require $assignable(x, $typeof(y[0]))
 **/
macro sincos(x, y)
{
	$if @typeid(y[0]) == float.typeid:
		return _sincosf(x, y);
	$else
		return _sincos(x, y);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro atan(x)
{
	$if @typeid(x) == float.typeid:
		return _atanf(x);
	$else
		return _atan(x);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro atanh(x)
{
	$if @typeid(x) == float.typeid:
		return _atanhf(x);
	$else
		return _atanh(x);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro acos(x)
{
	$if @typeid(x) == float.typeid:
		return _acosf(x);
	$else
		return _acos(x);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro acosh(x)
{
	$if @typeid(x) == float.typeid:
		return _acoshf(x);
	$else
		return _acosh(x);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro asin(x)
{
	$if @typeid(x) == float.typeid:
		return _asinf(x);
	$else
		return _asin(x);
	$endif
}

/**
 * @require values::@is_int(x) || values::@is_float(x) "Expected an integer or floating point value"
 **/
macro asinh(x)
{
	$if @typeid(x) == float.typeid:
		return _asinhf(x);
	$else
		return _asinh(x);
	$endif
}

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro ceil(x) => $$ceil(x);

/**
 * Constrain the value to lie within the given interval.
 *
 * @param x "the value to clamp, may be a number or a numerical vector."
 * @param lower "the lower bounds"
 * @param upper "the upper bounds"
 * @return "lower if x < lower, upper if x > upper, otherwise return x."
 *
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require values::@assign_to(lower, x) `The lower bound must be convertable to the value type.`
 * @require values::@assign_to(upper, x) `The upper bound must be convertable to the value type.`
 **/
macro clamp(x, lower, upper) => $$max(($typeof(x))lower, $$min(x, ($typeof(x))upper));

/**
 * @require values::@is_promotable_to_floatlike(mag) `The input must be a number value or float vector`
 * @require values::@assign_to(sgn, values::promote_int(mag))
 **/
macro copysign(mag, sgn) => $$copysign(values::promote_int_same(mag, sgn), ($typeof(values::promote_int_same(mag, sgn)))sgn);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cos(x) => $$cos(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosec(x) => 1 / sin(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosech(x) => 2 / (exp(x) - exp(-x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cosh(x) => (exp(x) + exp(-x)) / 2.0;

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cotan(x) => cos(x) / sin(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro cotanh(x) => (exp(2.0 * x) + 1.0) / (exp(2.0 * x) - 1.0);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro exp(x) => $$exp(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro exp2(x) => $$exp2(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number value or float vector`
 **/
macro floor(x) => $$floor(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(a) `The input must be a number or float vector`
 * @require values::@is_promotable_to_floatlike(b) `The input must be a number or float vector`
 * @require values::@is_promotable_to_floatlike(c) `The input must be a number or float vector`
 * @require values::@is_same_vector_type(a, b) `The input types must be equal`
 * @require values::@is_same_vector_type(a, c) `The input types must match`
 **/
macro fma(a, b, c) => $$fma(a, b, c);


/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 * @require values::@is_promotable_to_floatlike(y) `The input must be a number or a float vector`
 * @require values::@is_same_vector_type(x, y) `The input types must match`
 **/
macro hypot(x, y) => sqrt(sqr(x) + sqr(y));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro ln(x) => $$log(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 * @require values::@is_promotable_to_floatlike(base) `The base must be a number or a float vector`
 **/
macro log(x, base)
{
	return $$log(values::promote_int_same(x, base)) / $$log(values::promote_int_same(base, x));
}

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro log2(x) => $$log2(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro log10(x) => $$log10(values::promote_int(x));

/**
 * @require types::is_numerical($typeof(x)) `The input must be a floating point value or float vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro max(x, y, ...)
{
	$if $vacount == 0:
		return $$max(x, y);
	$else
		var m = $$max(x, y);
		$for (var $i = 0; $i < $vacount; $i++)
			m = $$max(m, $vaarg[$i]);
		$endfor
		return m;
	$endif
}

/**
 * @require types::is_numerical($typeof(x)) `The input must be a numerical value or numerical vector`
 * @require types::is_same($typeof(x), $typeof(y)) `The input types must be equal`
 **/
macro min(x, y, ...)
{
	$if $vacount == 0:
		return $$min(x, y);
	$else
		var m = $$min(x, y);
		$for (var $i = 0; $i < $vacount; $i++)
			m = $$min(m, $vaarg[$i]);
		$endfor
		return m;
	$endif
}

/**
 * @require types::@is_float(a) `The input must be a floating point value`
 * @require types::@has_same(a, b, c) `The input types must be equal`
 **/
macro muladd(a, b, c) => $$fmuladd(a, b, c);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro nearbyint(x) => $$nearbyint(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 * @require $assignable(exp, $typeof(values::promote_int(x))) || values::@is_int(exp) `The input must be an integer, castable to the type of x`
 **/
macro pow(x, exp)
{
	$if types::is_floatlike($typeof(exp)):
		return $$pow(values::promote_int_same(x, exp), ($typeof(values::promote_int_same(x, exp)))exp);
	$else
		return $$pow_int(values::promote_int(x), exp);
	$endif
}

/**
 * @require values::@is_promotable_to_float(x) : `The input must be integer or floating type`
 **/
macro frexp(x, int* e)
{
	$switch ($typeof(x))
	$case float:
	$case float16:
		return _frexpf((float)x, e);
	$default:
		return _frexp((double)x, e);
	$endswitch
}

/**
 * @require values::@is_promotable_to_float(x) : `The input must be integer or floating type`
 **/
macro int signbit(x)
{
	$switch ($typeof(x))
	$case float:
	$case float16:
		return bitcast((float)x, uint) >> 31;
	$default:
		return (int)(bitcast((double)x, ulong) >> 63);
	$endswitch
}

/**
 * @require values::@is_floatlike(x) `The input must be a number or a float vector`
 **/
macro rint(x) => $$rint(x);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro round(x) => $$round(x);


/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro round_to_decimals(x, int decimal_places)
{
  var div = $$pow_int(($typeof(x))10, decimal_places);
  return round(div * x) / div;
}

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro roundeven(x) => $$roundeven(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sec(x) => 1 / cos(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sech(x) => 2 / (exp(x) + exp(-x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sin(x) => $$sin(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sinh(x) => (exp(x) - exp(-x)) / 2.0;

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sqr(x) => values::promote_int(x) * values::promote_int(x);

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro sqrt(x) => $$sqrt(values::promote_int(x));

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro tan(x)
{
	var $Type = $typeof(x);
	$switch
		$case types::is_vector($Type):
			return $$sin(x) / $$cos(x);
		$case $Type.typeid == float.typeid:
			return _tanf(x);
		$default:
			return _tan(x);
	$endswitch
}

/**
 * @require values::@is_promotable_to_float(x) `The input must be a float`
 **/
macro bool is_finite(x)
{
	$switch ($typeof(x))
		$case float:
		$case float16:
			return bitcast((float)x, uint) & 0x7fffffff < 0x7f800000;
		$default:
			return bitcast((double)x, ulong) & (~0u64 >> 1) < 0x7ffu64 << 52;
	$endswitch
}

/**
 * @require values::@is_promotable_to_float(x) `The input must be a float`
 **/
macro is_nan(x)
{
	$switch ($typeof(x))
	$case float:
	$case float16:
		return bitcast((float)x, uint) & 0x7fffffff > 0x7f800000;
	$default:
		return bitcast((double)x, ulong) & (~0u64 >> 1) > 0x7ffu64 << 52;
	$endswitch
}

/**
 * @require values::@is_promotable_to_float(x) `The input must be a float`
 **/
macro is_inf(x)
{
	$switch ($typeof(x))
	$case float:
	$case float16:
		return bitcast((float)x, uint) & 0x7fffffff == 0x7f800000;
	$default:
		return bitcast((double)x, ulong) & (~0u64 >> 1) == 0x7ffu64 << 52;
	$endswitch
}

/**
 * @require values::@is_promotable_to_floatlike(x) `The input must be a number or a float vector`
 **/
macro tanh(x) => (exp(2.0 * x) - 1.0) / (exp(2.0 * x) + 1.0);

/**
 * @require values::@is_floatlike(x) `The input must be a floating point value or float vector`
 **/
macro trunc(x) => $$trunc(x);

macro lerp(x, y, amount) @private => x + (y - x) * amount;
macro reflect(x, y) @private
{
	var dot = x.dot(y);
	return x - 2 * y * dot;
}
macro normalize(x) @private
{
	var len = x.length();
	if (len == 0) return x;
	return x * (1 / len);
}

/**
 * Use a mask to select values from either "then" or "else" vectors.
 *
 * @param mask "The mask to use for the select, 'true' will pick the then_value, 'false' the else_value"
 * @param then_value "The vector to get elements from where the mask is 'true'"
 * @param else_value "The vector to get elements from where the mask is 'false'"
 * @require values::@is_vector(then_value) && values::@is_vector(else_value) "'Then' and 'else' must be vectors."
 * @require values::@is_same_type(then_value, else_value) "'Then' and 'else' vectors must be of the same type."
 * @require then_value.len == mask.len "Mask and selected vectors must be of the same width."
 *
 * @return "a vector of the same type as then/else"
 **/
macro select(bool[<*>] mask, then_value, else_value)
{
	return $$select(mask, then_value, else_value);
}

macro float float.ceil(float x) => $$ceil(x);
macro float float.clamp(float x, float lower, float upper) => $$max(lower, $$min(x, upper));
macro float float.copysign(float mag, float sgn) => $$copysign(mag, sgn);
macro float float.floor(float x) => $$floor(x);
macro float float.fma(float a, float b, float c) => $$fma(a, b, c);
macro float float.muladd(float a, float b, float c) => $$fmuladd(a, b, c);
macro float float.nearbyint(float x) => $$nearbyint(x);
macro float float.pow(float x, exp) => pow(x, exp);
macro float float.rint(float x) => $$rint(x);
macro float float.round(float x) => $$round(x);
macro float float.roundeven(float x) => $$roundeven(x);
macro float float.trunc(float x) => $$trunc(x);

macro float float[<*>].sum(float[<*>] x, float start = 0.0) => $$reduce_fadd(x, start);
macro float float[<*>].product(float[<*>] x, float start = 1.0) => $$reduce_fmul(x, start);
macro float float[<*>].max(float[<*>] x) => $$reduce_max(x);
macro float float[<*>].min(float[<*>] x) => $$reduce_min(x);
macro float[<*>] float[<*>].ceil(float[<*>] x) => $$ceil(x);
macro float[<*>] float[<*>].clamp(float[<*>] x, float[<*>] lower, float[<*>] upper) => $$max(lower, $$min(x, upper));
macro float[<*>] float[<*>].copysign(float[<*>] mag, float[<*>] sgn) => $$copysign(mag, sgn);
macro float[<*>] float[<*>].fma(float[<*>] a, float[<*>] b, float[<*>] c) => $$fma(a, b, c);
macro float[<*>] float[<*>].floor(float[<*>] x) => $$floor(x);
macro float[<*>] float[<*>].nearbyint(float[<*>] x) => $$nearbyint(x);
macro float[<*>] float[<*>].pow(float[<*>] x, exp) => pow(x, exp);
macro float[<*>] float[<*>].rint(float[<*>] x) => $$rint(x);
macro float[<*>] float[<*>].round(float[<*>] x) => $$round(x);
macro float[<*>] float[<*>].roundeven(float[<*>] x) => $$roundeven(x);
macro float[<*>] float[<*>].trunc(float[<*>] x) => $$trunc(x);
macro float float[<*>].dot(float[<*>] x, float[<*>] y) => (x * y).sum();
macro float float[<*>].length(float[<*>] x) => $$sqrt(x.dot(x));
macro float float[<*>].distance(float[<*>] x, float[<*>] y) => (x - y).length();
macro float[<*>] float[<*>].normalize(float[<*>] x) => normalize(x);
macro float[<*>] float[<*>].lerp(float[<*>] x, float[<*>] y, float amount) => lerp(x, y, amount);
macro float[<*>] float[<*>].reflect(float[<*>] x, float[<*>] y) => reflect(x, y);
macro bool float[<*>].equals(float[<*>] x, float[<*>] y) => equals_vec(x, y);

macro bool[<*>] float[<*>].comp_lt(float[<*>] x, float[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] float[<*>].comp_le(float[<*>] x, float[<*>] y) => $$veccomple(x, y);
macro bool[<*>] float[<*>].comp_eq(float[<*>] x, float[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] float[<*>].comp_gt(float[<*>] x, float[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] float[<*>].comp_ge(float[<*>] x, float[<*>] y) => $$veccompge(x, y);
macro bool[<*>] float[<*>].comp_ne(float[<*>] x, float[<*>] y) => $$veccompne(x, y);

macro double double.ceil(double x) => $$ceil(x);
macro double double.clamp(double x, double lower, double upper) => $$max(lower, $$min(x, upper));
macro double double.copysign(double mag, double sgn) => $$copysign(mag, sgn);
macro double double.floor(double x) => $$floor(x);
macro double double.fma(double a, double b, double c) => $$fma(a, b, c);
macro double double.muladd(double a, double b, double c) => $$fmuladd(a, b, c);
macro double double.nearbyint(double x) => $$nearbyint(x);
macro double double.pow(double x, exp) => pow(x, exp);
macro double double.rint(double x) => $$rint(x);
macro double double.round(double x) => $$round(x);
macro double double.roundeven(double x) => $$roundeven(x);
macro double double.trunc(double x) => $$trunc(x);

macro double double[<*>].sum(double[<*>] x, double start = 0.0) => $$reduce_fadd(x, start);
macro double double[<*>].product(double[<*>] x, double start = 1.0) => $$reduce_fmul(x, start);
macro double double[<*>].max(double[<*>] x) => $$reduce_fmax(x);
macro double double[<*>].min(double[<*>] x) => $$reduce_fmin(x);
macro double[<*>] double[<*>].ceil(double[<*>] x) => $$ceil(x);
macro double[<*>] double[<*>].clamp(double[<*>] x, double[<*>] lower, double[<*>] upper) => $$max(lower, $$min(x, upper));
macro double[<*>] double[<*>].copysign(double[<*>] mag, double[<*>] sgn) => $$copysign(mag, sgn);
macro double[<*>] double[<*>].floor(double[<*>] x) => $$floor(x);
macro double[<*>] double[<*>].fma(double[<*>] a, double[<*>] b, double[<*>] c) => $$fma(a, b, c);
macro double[<*>] double[<*>].nearbyint(double[<*>] x) => $$nearbyint(x);
macro double[<*>] double[<*>].pow(double[<*>] x, exp) => pow(x, exp);
macro double[<*>] double[<*>].rint(double[<*>] x) => $$rint(x);
macro double[<*>] double[<*>].round(double[<*>] x) => $$round(x);
macro double[<*>] double[<*>].roundeven(double[<*>] x) => $$roundeven(x);
macro double[<*>] double[<*>].trunc(double[<*>] x) => $$trunc(x);
macro double double[<*>].dot(double[<*>] x, double[<*>] y) => (x * y).sum();
macro double double[<*>].length(double[<*>] x) => $$sqrt(x.dot(x));
macro double double[<*>].distance(double[<*>] x, double[<*>] y) => (x - y).length();
macro double[<*>] double[<*>].normalize(double[<*>] x) => normalize(x);
macro double[<*>] double[<*>].reflect(double[<*>] x, double[<*>] y) => reflect(x, y);
macro double[<*>] double[<*>].lerp(double[<*>] x, double[<*>] y, double amount) => lerp(x, y, amount);
macro bool double[<*>].equals(double[<*>] x, double[<*>] y) => equals_vec(x, y);

macro bool[<*>] double[<*>].comp_lt(double[<*>] x, double[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] double[<*>].comp_le(double[<*>] x, double[<*>] y) => $$veccomple(x, y);
macro bool[<*>] double[<*>].comp_eq(double[<*>] x, double[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] double[<*>].comp_gt(double[<*>] x, double[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] double[<*>].comp_ge(double[<*>] x, double[<*>] y) => $$veccompge(x, y);
macro bool[<*>] double[<*>].comp_ne(double[<*>] x, double[<*>] y) => $$veccompne(x, y);

macro bool[<*>] ichar[<*>].comp_lt(ichar[<*>] x, ichar[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ichar[<*>].comp_le(ichar[<*>] x, ichar[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ichar[<*>].comp_eq(ichar[<*>] x, ichar[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ichar[<*>].comp_gt(ichar[<*>] x, ichar[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ichar[<*>].comp_ge(ichar[<*>] x, ichar[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ichar[<*>].comp_ne(ichar[<*>] x, ichar[<*>] y) => $$veccompne(x, y);

macro ichar ichar[<*>].sum(ichar[<*>] x) => $$reduce_add(x);
macro ichar ichar[<*>].product(ichar[<*>] x) => $$reduce_mul(x);
macro ichar ichar[<*>].and(ichar[<*>] x) => $$reduce_and(x);
macro ichar ichar[<*>].or(ichar[<*>] x) => $$reduce_or(x);
macro ichar ichar[<*>].xor(ichar[<*>] x) => $$reduce_xor(x);
macro ichar ichar[<*>].max(ichar[<*>] x) => $$reduce_max(x);
macro ichar ichar[<*>].min(ichar[<*>] x) => $$reduce_min(x);
macro ichar ichar[<*>].dot(ichar[<*>] x, ichar[<*>] y) => (x * y).sum();

macro bool[<*>] short[<*>].comp_lt(short[<*>] x, short[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] short[<*>].comp_le(short[<*>] x, short[<*>] y) => $$veccomple(x, y);
macro bool[<*>] short[<*>].comp_eq(short[<*>] x, short[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] short[<*>].comp_gt(short[<*>] x, short[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] short[<*>].comp_ge(short[<*>] x, short[<*>] y) => $$veccompge(x, y);
macro bool[<*>] short[<*>].comp_ne(short[<*>] x, short[<*>] y) => $$veccompne(x, y);

macro short short[<*>].sum(short[<*>] x) => $$reduce_add(x);
macro short short[<*>].product(short[<*>] x) => $$reduce_mul(x);
macro short short[<*>].and(short[<*>] x) => $$reduce_and(x);
macro short short[<*>].or(short[<*>] x) => $$reduce_or(x);
macro short short[<*>].xor(short[<*>] x) => $$reduce_xor(x);
macro short short[<*>].max(short[<*>] x) => $$reduce_max(x);
macro short short[<*>].min(short[<*>] x) => $$reduce_min(x);
macro short short[<*>].dot(short[<*>] x, short[<*>] y) => (x * y).sum();

macro bool[<*>] int[<*>].comp_lt(int[<*>] x, int[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] int[<*>].comp_le(int[<*>] x, int[<*>] y) => $$veccomple(x, y);
macro bool[<*>] int[<*>].comp_eq(int[<*>] x, int[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] int[<*>].comp_gt(int[<*>] x, int[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] int[<*>].comp_ge(int[<*>] x, int[<*>] y) => $$veccompge(x, y);
macro bool[<*>] int[<*>].comp_ne(int[<*>] x, int[<*>] y) => $$veccompne(x, y);

macro int int[<*>].sum(int[<*>] x) => $$reduce_add(x);
macro int int[<*>].product(int[<*>] x) => $$reduce_mul(x);
macro int int[<*>].and(int[<*>] x) => $$reduce_and(x);
macro int int[<*>].or(int[<*>] x) => $$reduce_or(x);
macro int int[<*>].xor(int[<*>] x) => $$reduce_xor(x);
macro int int[<*>].max(int[<*>] x) => $$reduce_max(x);
macro int int[<*>].min(int[<*>] x) => $$reduce_min(x);
macro int int[<*>].dot(int[<*>] x, int[<*>] y) => (x * y).sum();

macro bool[<*>] long[<*>].comp_lt(long[<*>] x, long[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] long[<*>].comp_le(long[<*>] x, long[<*>] y) => $$veccomple(x, y);
macro bool[<*>] long[<*>].comp_eq(long[<*>] x, long[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] long[<*>].comp_gt(long[<*>] x, long[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] long[<*>].comp_ge(long[<*>] x, long[<*>] y) => $$veccompge(x, y);
macro bool[<*>] long[<*>].comp_ne(long[<*>] x, long[<*>] y) => $$veccompne(x, y);
macro long long[<*>].sum(long[<*>] x) => $$reduce_add(x);
macro long long[<*>].product(long[<*>] x) => $$reduce_mul(x);
macro long long[<*>].and(long[<*>] x) => $$reduce_and(x);
macro long long[<*>].or(long[<*>] x) => $$reduce_or(x);
macro long long[<*>].xor(long[<*>] x) => $$reduce_xor(x);
macro long long[<*>].max(long[<*>] x) => $$reduce_max(x);
macro long long[<*>].min(long[<*>] x) => $$reduce_min(x);
macro long long[<*>].dot(long[<*>] x, long[<*>] y) => (x * y).sum();

macro bool[<*>] int128[<*>].comp_lt(int128[<*>] x, int128[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] int128[<*>].comp_le(int128[<*>] x, int128[<*>] y) => $$veccomple(x, y);
macro bool[<*>] int128[<*>].comp_eq(int128[<*>] x, int128[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] int128[<*>].comp_gt(int128[<*>] x, int128[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] int128[<*>].comp_ge(int128[<*>] x, int128[<*>] y) => $$veccompge(x, y);
macro bool[<*>] int128[<*>].comp_ne(int128[<*>] x, int128[<*>] y) => $$veccompne(x, y);
macro int128 int128[<*>].sum(int128[<*>] x) => $$reduce_add(x);
macro int128 int128[<*>].product(int128[<*>] x) => $$reduce_mul(x);
macro int128 int128[<*>].and(int128[<*>] x) => $$reduce_and(x);
macro int128 int128[<*>].or(int128[<*>] x) => $$reduce_or(x);
macro int128 int128[<*>].xor(int128[<*>] x) => $$reduce_xor(x);
macro int128 int128[<*>].max(int128[<*>] x) => $$reduce_max(x);
macro int128 int128[<*>].min(int128[<*>] x) => $$reduce_min(x);
macro int128 int128[<*>].dot(int128[<*>] x, int128[<*>] y) => (x * y).sum();

macro bool[<*>] bool[<*>].comp_lt(bool[<*>] x, bool[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] bool[<*>].comp_le(bool[<*>] x, bool[<*>] y) => $$veccomple(x, y);
macro bool[<*>] bool[<*>].comp_eq(bool[<*>] x, bool[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] bool[<*>].comp_gt(bool[<*>] x, bool[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] bool[<*>].comp_ge(bool[<*>] x, bool[<*>] y) => $$veccompge(x, y);
macro bool[<*>] bool[<*>].comp_ne(bool[<*>] x, bool[<*>] y) => $$veccompne(x, y);

macro bool bool[<*>].sum(bool[<*>] x) => $$reduce_add(x);
macro bool bool[<*>].product(bool[<*>] x) => $$reduce_mul(x);
macro bool bool[<*>].and(bool[<*>] x) => $$reduce_and(x);
macro bool bool[<*>].or(bool[<*>] x) => $$reduce_or(x);
macro bool bool[<*>].xor(bool[<*>] x) => $$reduce_xor(x);
macro bool bool[<*>].max(bool[<*>] x) => $$reduce_max(x);
macro bool bool[<*>].min(bool[<*>] x) => $$reduce_min(x);

macro bool[<*>] char[<*>].comp_lt(char[<*>] x, char[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] char[<*>].comp_le(char[<*>] x, char[<*>] y) => $$veccomple(x, y);
macro bool[<*>] char[<*>].comp_eq(char[<*>] x, char[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] char[<*>].comp_gt(char[<*>] x, char[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] char[<*>].comp_ge(char[<*>] x, char[<*>] y) => $$veccompge(x, y);
macro bool[<*>] char[<*>].comp_ne(char[<*>] x, char[<*>] y) => $$veccompne(x, y);

macro char char[<*>].sum(char[<*>] x) => $$reduce_add(x);
macro char char[<*>].product(char[<*>] x) => $$reduce_mul(x);
macro char char[<*>].and(char[<*>] x) => $$reduce_and(x);
macro char char[<*>].or(char[<*>] x) => $$reduce_or(x);
macro char char[<*>].xor(char[<*>] x) => $$reduce_xor(x);
macro char char[<*>].max(char[<*>] x) => $$reduce_max(x);
macro char char[<*>].min(char[<*>] x) => $$reduce_min(x);
macro char char[<*>].dot(char[<*>] x, char[<*>] y) => (x * y).sum();

macro bool[<*>] ushort[<*>].comp_lt(ushort[<*>] x, ushort[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ushort[<*>].comp_le(ushort[<*>] x, ushort[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ushort[<*>].comp_eq(ushort[<*>] x, ushort[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ushort[<*>].comp_gt(ushort[<*>] x, ushort[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ushort[<*>].comp_ge(ushort[<*>] x, ushort[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ushort[<*>].comp_ne(ushort[<*>] x, ushort[<*>] y) => $$veccompne(x, y);

macro ushort ushort[<*>].sum(ushort[<*>] x) => $$reduce_add(x);
macro ushort ushort[<*>].product(ushort[<*>] x) => $$reduce_mul(x);
macro ushort ushort[<*>].and(ushort[<*>] x) => $$reduce_and(x);
macro ushort ushort[<*>].or(ushort[<*>] x) => $$reduce_or(x);
macro ushort ushort[<*>].xor(ushort[<*>] x) => $$reduce_xor(x);
macro ushort ushort[<*>].max(ushort[<*>] x) => $$reduce_max(x);
macro ushort ushort[<*>].min(ushort[<*>] x) => $$reduce_min(x);
macro ushort ushort[<*>].dot(ushort[<*>] x, ushort[<*>] y) => (x * y).sum();

macro bool[<*>] uint[<*>].comp_lt(uint[<*>] x, uint[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] uint[<*>].comp_le(uint[<*>] x, uint[<*>] y) => $$veccomple(x, y);
macro bool[<*>] uint[<*>].comp_eq(uint[<*>] x, uint[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] uint[<*>].comp_gt(uint[<*>] x, uint[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] uint[<*>].comp_ge(uint[<*>] x, uint[<*>] y) => $$veccompge(x, y);
macro bool[<*>] uint[<*>].comp_ne(uint[<*>] x, uint[<*>] y) => $$veccompne(x, y);

macro uint uint[<*>].sum(uint[<*>] x) => $$reduce_add(x);
macro uint uint[<*>].product(uint[<*>] x) => $$reduce_mul(x);
macro uint uint[<*>].and(uint[<*>] x) => $$reduce_and(x);
macro uint uint[<*>].or(uint[<*>] x) => $$reduce_or(x);
macro uint uint[<*>].xor(uint[<*>] x) => $$reduce_xor(x);
macro uint uint[<*>].max(uint[<*>] x) => $$reduce_max(x);
macro uint uint[<*>].min(uint[<*>] x) => $$reduce_min(x);
macro uint uint[<*>].dot(uint[<*>] x, uint[<*>] y) => (x * y).sum();

macro bool[<*>] ulong[<*>].comp_lt(ulong[<*>] x, ulong[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] ulong[<*>].comp_le(ulong[<*>] x, ulong[<*>] y) => $$veccomple(x, y);
macro bool[<*>] ulong[<*>].comp_eq(ulong[<*>] x, ulong[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] ulong[<*>].comp_gt(ulong[<*>] x, ulong[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] ulong[<*>].comp_ge(ulong[<*>] x, ulong[<*>] y) => $$veccompge(x, y);
macro bool[<*>] ulong[<*>].comp_ne(ulong[<*>] x, ulong[<*>] y) => $$veccompne(x, y);

macro ulong ulong[<*>].sum(ulong[<*>] x) => $$reduce_add(x);
macro ulong ulong[<*>].product(ulong[<*>] x) => $$reduce_mul(x);
macro ulong ulong[<*>].and(ulong[<*>] x) => $$reduce_and(x);
macro ulong ulong[<*>].or(ulong[<*>] x) => $$reduce_or(x);
macro ulong ulong[<*>].xor(ulong[<*>] x) => $$reduce_xor(x);
macro ulong ulong[<*>].max(ulong[<*>] x) => $$reduce_max(x);
macro ulong ulong[<*>].min(ulong[<*>] x) => $$reduce_min(x);
macro ulong ulong[<*>].dot(ulong[<*>] x, ulong[<*>] y) => (x * y).sum();

macro bool[<*>] uint128[<*>].comp_lt(uint128[<*>] x, uint128[<*>] y) => $$veccomplt(x, y);
macro bool[<*>] uint128[<*>].comp_le(uint128[<*>] x, uint128[<*>] y) => $$veccomple(x, y);
macro bool[<*>] uint128[<*>].comp_eq(uint128[<*>] x, uint128[<*>] y) => $$veccompeq(x, y);
macro bool[<*>] uint128[<*>].comp_gt(uint128[<*>] x, uint128[<*>] y) => $$veccompgt(x, y);
macro bool[<*>] uint128[<*>].comp_ge(uint128[<*>] x, uint128[<*>] y) => $$veccompge(x, y);
macro bool[<*>] uint128[<*>].comp_ne(uint128[<*>] x, uint128[<*>] y) => $$veccompne(x, y);

macro uint128 uint128[<*>].sum(uint128[<*>] x) => $$reduce_add(x);
macro uint128 uint128[<*>].product(uint128[<*>] x) => $$reduce_mul(x);
macro uint128 uint128[<*>].and(uint128[<*>] x) => $$reduce_and(x);
macro uint128 uint128[<*>].or(uint128[<*>] x) => $$reduce_or(x);
macro uint128 uint128[<*>].xor(uint128[<*>] x) => $$reduce_xor(x);
macro uint128 uint128[<*>].max(uint128[<*>] x) => $$reduce_max(x);
macro uint128 uint128[<*>].min(uint128[<*>] x) => $$reduce_min(x);
macro uint128 uint128[<*>].dot(uint128[<*>] x, uint128[<*>] y) => (x * y).sum();

macro char char.sat_add(char x, char y) => $$sat_add(x, y);
macro char char.sat_sub(char x, char y) => $$sat_sub(x, y);
macro char char.sat_mul(char x, char y) => $$sat_mul(x, y);
macro char char.sat_shl(char x, char y) => $$sat_shl(x, y);
macro char! char.overflow_add(char x, char y) => overflow_add_helper(x, y);
macro char! char.overflow_sub(char x, char y) => overflow_sub_helper(x, y);
macro char! char.overflow_mul(char x, char y) => overflow_mul_helper(x, y);


macro ichar ichar.sat_add(ichar x, ichar y) => $$sat_add(x, y);
macro ichar ichar.sat_sub(ichar x, ichar y) => $$sat_sub(x, y);
macro ichar ichar.sat_mul(ichar x, ichar y) => $$sat_mul(x, y);
macro ichar ichar.sat_shl(ichar x, ichar y) => $$sat_shl(x, y);
macro ichar! ichar.overflow_add(ichar x, ichar y) => overflow_add_helper(x, y);
macro ichar! ichar.overflow_sub(ichar x, ichar y) => overflow_sub_helper(x, y);
macro ichar! ichar.overflow_mul(ichar x, ichar y) => overflow_mul_helper(x, y);

macro ushort ushort.sat_add(ushort x, ushort y) => $$sat_add(x, y);
macro ushort ushort.sat_sub(ushort x, ushort y) => $$sat_sub(x, y);
macro ushort ushort.sat_mul(ushort x, ushort y) => $$sat_mul(x, y);
macro ushort ushort.sat_shl(ushort x, ushort y) => $$sat_shl(x, y);
macro ushort! ushort.overflow_add(ushort x, ushort y) => overflow_add_helper(x, y);
macro ushort! ushort.overflow_sub(ushort x, ushort y) => overflow_sub_helper(x, y);
macro ushort! ushort.overflow_mul(ushort x, ushort y) => overflow_mul_helper(x, y);

macro short short.sat_add(short x, short y) => $$sat_add(x, y);
macro short short.sat_sub(short x, short y) => $$sat_sub(x, y);
macro short short.sat_mul(short x, short y) => $$sat_mul(x, y);
macro short short.sat_shl(short x, short y) => $$sat_shl(x, y);
macro short! short.overflow_add(short x, short y) => overflow_add_helper(x, y);
macro short! short.overflow_sub(short x, short y) => overflow_sub_helper(x, y);
macro short! short.overflow_mul(short x, short y) => overflow_mul_helper(x, y);

macro uint uint.sat_add(uint x, uint y) => $$sat_add(x, y);
macro uint uint.sat_sub(uint x, uint y) => $$sat_sub(x, y);
macro uint uint.sat_mul(uint x, uint y) => $$sat_mul(x, y);
macro uint uint.sat_shl(uint x, uint y) => $$sat_shl(x, y);
macro uint! uint.overflow_add(uint x, uint y) => overflow_add_helper(x, y);
macro uint! uint.overflow_sub(uint x, uint y) => overflow_sub_helper(x, y);
macro uint! uint.overflow_mul(uint x, uint y) => overflow_mul_helper(x, y);

macro int int.sat_add(int x, int y) => $$sat_add(x, y);
macro int int.sat_sub(int x, int y) => $$sat_sub(x, y);
macro int int.sat_mul(int x, int y) => $$sat_mul(x, y);
macro int int.sat_shl(int x, int y) => $$sat_shl(x, y);
macro int! int.overflow_add(int x, int y) => overflow_add_helper(x, y);
macro int! int.overflow_sub(int x, int y) => overflow_sub_helper(x, y);
macro int! int.overflow_mul(int x, int y) => overflow_mul_helper(x, y);

macro ulong ulong.sat_add(ulong x, ulong y) => $$sat_add(x, y);
macro ulong ulong.sat_sub(ulong x, ulong y) => $$sat_sub(x, y);
macro ulong ulong.sat_mul(ulong x, ulong y) => $$sat_mul(x, y);
macro ulong ulong.sat_shl(ulong x, ulong y) => $$sat_shl(x, y);
macro ulong! ulong.overflow_add(ulong x, ulong y) => overflow_add_helper(x, y);
macro ulong! ulong.overflow_sub(ulong x, ulong y) => overflow_sub_helper(x, y);
macro ulong! ulong.overflow_mul(ulong x, ulong y) => overflow_mul_helper(x, y);

macro long long.sat_add(long x, long y) => $$sat_add(x, y);
macro long long.sat_sub(long x, long y) => $$sat_sub(x, y);
macro long long.sat_mul(long x, long y) => $$sat_mul(x, y);
macro long long.sat_shl(long x, long y) => $$sat_shl(x, y);
macro long! long.overflow_add(long x, long y) => overflow_add_helper(x, y);
macro long! long.overflow_sub(long x, long y) => overflow_sub_helper(x, y);
macro long! long.overflow_mul(long x, long y) => overflow_mul_helper(x, y);

macro uint128 uint128.sat_add(uint128 x, uint128 y) => $$sat_add(x, y);
macro uint128 uint128.sat_sub(uint128 x, uint128 y) => $$sat_sub(x, y);
macro uint128 uint128.sat_mul(uint128 x, uint128 y) => $$sat_mul(x, y);
macro uint128 uint128.sat_shl(uint128 x, uint128 y) => $$sat_shl(x, y);
macro uint128! uint128.overflow_add(uint128 x, uint128 y) => overflow_add_helper(x, y);
macro uint128! uint128.overflow_sub(uint128 x, uint128 y) => overflow_sub_helper(x, y);
macro uint128! uint128.overflow_mul(uint128 x, uint128 y) => overflow_mul_helper(x, y);

macro int128 int128.sat_add(int128 x, int128 y) => $$sat_add(x, y);
macro int128 int128.sat_sub(int128 x, int128 y) => $$sat_sub(x, y);
macro int128 int128.sat_mul(int128 x, int128 y) => $$sat_mul(x, y);
macro int128 int128.sat_shl(int128 x, int128 y) => $$sat_shl(x, y);
macro int128! int128.overflow_add(int128 x, int128 y) => overflow_add_helper(x, y);
macro int128! int128.overflow_sub(int128 x, int128 y) => overflow_sub_helper(x, y);
macro int128! int128.overflow_mul(int128 x, int128 y) => overflow_mul_helper(x, y);
/**
 * @require values::@is_int(x) `The input must be an integer`
 **/
macro bool is_odd(x) => (bool)(x & 1);

/**
 * @require values::@is_int(x) `The input must be an integer`
 **/
macro bool is_even(x) => !is_odd(x);

macro bool char.is_even(char x) => is_even(x);
macro bool char.is_odd(char x) => is_odd(x);

macro bool ichar.is_even(ichar x) => is_even(x);
macro bool ichar.is_odd(ichar x) => is_odd(x);

macro bool ushort.is_even(ushort x) => is_even(x);
macro bool ushort.is_odd(ushort x) => is_odd(x);

macro bool short.is_even(short x) => is_even(x);
macro bool short.is_odd(short x) => is_odd(x);

macro bool uint.is_even(uint x) => is_even(x);
macro bool uint.is_odd(uint x) => is_odd(x);

macro bool int.is_even(int x) => is_even(x);
macro bool int.is_odd(int x) => is_odd(x);

macro bool ulong.is_even(ulong x) => is_even(x);
macro bool ulong.is_odd(ulong x) => is_odd(x);

macro bool long.is_even(long x) => is_even(x);
macro bool long.is_odd(long x) => is_odd(x);

macro bool uint128.is_even(uint128 x) => is_even(x);
macro bool uint128.is_odd(uint128 x) => is_odd(x);

macro bool int128.is_even(int128 x) => is_even(x);
macro bool int128.is_odd(int128 x) => is_odd(x);

/**
 * @require types::is_underlying_int($typeof(x)) : `is_power_of_2 may only be used on integer types`
 */
macro bool is_power_of_2(x)
{
	return x != 0 && (x & (x - 1)) == 0;
}

macro next_power_of_2(x)
{
	$typeof(x) y = 1;
	while (y < x) y += y;
	return y;
}

macro equals_vec(v1, v2) @private
{
	var $elements = v1.len;
	var abs_diff = math::abs(v1 - v2);
	var abs_v1 = math::abs(v1);
	var abs_v2 = math::abs(v2);
	$typeof(abs_v2) eps = 1;
	return abs_diff.comp_le(FLOAT_EPSILON * math::max(abs_v1, abs_v2, eps)).and();
}

macro uint double.high_word(double d) => (uint)(bitcast(d, ulong) >> 32);
macro uint double.low_word(double d) => (uint)bitcast(d, ulong);
macro uint float.word(float d) => bitcast(d, uint);


macro double scalbn(double x, int n) => _scalbn(x, n);

extern fn double _atan(double x) @extern("atan");
extern fn float _atanf(float x) @extern("atanf");
extern fn double _atan2(double, double) @extern("atan2");
extern fn float _atan2f(float, float) @extern("atan2f");
extern fn void _sincos(double, double*) @extern("sincos");
extern fn void _sincosf(float, float*) @extern("sincosf");
extern fn double _tan(double x) @extern("tan");
extern fn float _tanf(float x) @extern("tanf");
extern fn double _scalbn(double x, int n) @extern("scalbn");
extern fn double _acos(double x) @extern("acos");
extern fn double _asin(double x) @extern("asin");
extern fn double _acosh(double x) @extern("acosh");
extern fn double _asinh(double x) @extern("asinh");
extern fn double _atanh(double x) @extern("atanh");
extern fn float _acosf(float x) @extern("acosf");
extern fn float _asinf(float x) @extern("asinf");
extern fn float _acoshf(float x) @extern("acoshf");
extern fn float _asinhf(float x) @extern("asinhf");
extern fn float _atanhf(float x) @extern("atanhf");


fn double _frexp(double x, int* e)
{
	ulong i = bitcast(x, ulong);
	int ee = (int)((i >> 52) & 0x7ff);
	switch
	{
		case !ee:
			if (!x)
			{
				*e = 0;
				return x;
			}
			x = _frexp(x * 0x1p64, e);
			*e -= 64;
			return x;
		case ee == 0x7ff:
			return x;
		default:
			*e = ee - 0x3fe;
			i &= 0x800fffffffffffffu64;
			i |= 0x3fe0000000000000u64;
			return bitcast(i, double);
	}
}

fn float _frexpf(float x, int* e)
{
	uint i = bitcast(x, uint);
	int ee = (i >> 23) & 0xff;

	switch
	{
		case !ee:
			if (!x)
			{
				*e = 0;
				return x;
			}
			x = _frexpf(x * 0x1p64, e);
			*e -= 64;
			return x;
		case ee == 0xff:
			return x;
		default:
			*e = ee - 0x7e;
			i &= 0x807fffffu32;
			i |= 0x3f000000u32;
			return bitcast(i, float);
	}
}

macro overflow_add_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_add(x, y, &res)) return MathError.OVERFLOW?;
	return res;
}

macro overflow_sub_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_sub(x, y, &res)) return MathError.OVERFLOW?;
	return res;
}

macro overflow_mul_helper(x, y) @local
{
	$typeof(x) res @noinit;
	if ($$overflow_mul(x, y, &res)) return MathError.OVERFLOW?;
	return res;
}

macro mul_div_helper(val, mul, div) @private
{
	var $Type = $typeof(val);
	return ($Type)(($Type)mul * (val / ($Type)div) + ($Type)mul * (val % ($Type)div) / ($Type)div);
}
macro char char.muldiv(self, char mul, char div) => mul_div_helper(self, mul, div);
macro ichar ichar.muldiv(self, ichar mul, ichar div) => mul_div_helper(self, mul, div);
macro short short.muldiv(self, short mul, short div) => mul_div_helper(self, mul, div);
macro ushort ushort.muldiv(self, ushort mul, ushort div) => mul_div_helper(self, mul, div);
macro int int.muldiv(self, int mul, int div) => mul_div_helper(self, mul, div);
macro uint uint.muldiv(self, uint mul, uint div) => mul_div_helper(self, mul, div);
macro long long.muldiv(self, long mul, long div) => mul_div_helper(self, mul, div);
macro ulong ulong.muldiv(self, ulong mul, ulong div) => mul_div_helper(self, mul, div);

macro bool @is_same_vector_or_scalar(#vector_value, #vector_or_scalar) @private
{
	return (values::@is_vector(#vector_or_scalar) &&& values::@is_same_vector_type(#vector_value, #vector_or_scalar)) ||| values::@is_int(#vector_or_scalar);
}

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro char[<*>] char[<*>].muldiv(self, mul, div)  => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro ichar[<*>] ichar[<*>].muldiv(self, mul, div)  => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro short[<*>] short[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro ushort[<*>] ushort[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro int[<*>] int[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro uint[<*>] uint[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro long[<*>] long[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);

/**
 * @require @is_same_vector_or_scalar(self, mul) `mul must be a vector of the same type as self, or be an integer scalar`
 * @require @is_same_vector_or_scalar(self, div) `div must be a vector of the same type as self, or be an integer scalar`
 */
macro ulong[<*>] ulong[<*>].muldiv(self, mul, div) => mul_div_helper(self, mul, div);