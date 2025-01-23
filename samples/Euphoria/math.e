--****
-- == Math

namespace math

public include std/rand.e
public include std/mathcons.e
include std/error.e

type trig_range(object x)
--  values passed to arccos and arcsin must be [-1,+1]
	if atom(x) then
		return x >= -1 and x <= 1
	else
		for i = 1 to length(x) do
			if not trig_range(x[i]) then
				return 0
			end if
		end for
		return 1
	end if
end type

--****
-- === Sign and Comparisons
--

--**
-- returns the absolute value of numbers.
public function abs(object a)
	object t
	if atom(a) then
		if a >= 0 then
			return a
		else
			return - a
		end if
	end if
	for i = 1 to length(a) do
		t = a[i]
		if atom(t) then
			if t < 0 then
				a[i] = - t
			end if
		else
			a[i] = abs(t)
		end if
	end for
	return a
end function

--**
-- returns -1, 0 or 1 for each element according to it being negative, zero or positive.
public function sign(object a)
	-- small so normally it will be inlined
	return (a > 0) - (a < 0)
end function

--**
-- returns the larger of two objects.
public function larger_of(object objA, object objB)

	if compare(objA, objB) > 0 then
		return objA
	else
		return objB
	end if
end function

--**
-- returns the smaller of two objects.
public function smaller_of(object objA, object objB)
	if compare(objA, objB) < 0 then
		return objA
	else
		return objB
	end if
end function

--**
-- computes the maximum value among all the argument's elements.
public function max(object a)
	atom b, c
	if atom(a) then
		return a
	end if
	b = mathcons:MINF
	for i = 1 to length(a) do
		c = max(a[i])
		if c > b then
			b = c
		end if
	end for
	return b
end function

--**
-- computes the minimum value among all the argument's elements.
public function min(object a)
	atom b, c
	if atom(a) then
			return a
	end if
	b = mathcons:PINF
	for i = 1 to length(a) do
		c = min(a[i])
			if c < b then
				b = c
		end if
	end for
	return b
end function

--**
-- ensures that the ##item## is in a range of values supplied by inclusive ##range_limits##.
public function ensure_in_range(object item, sequence range_limits)
	if length(range_limits) < 2 then
		return item
	end if

	if eu:compare(item, range_limits[1]) < 0 then
		return range_limits[1]
	end if
	if eu:compare(item, range_limits[$]) > 0 then
		return range_limits[$]
	end if
	return item
end function

--**
-- ensures that the ##item## is in a list of values supplied by ##list##.
public function ensure_in_list(object item, sequence list, integer default=1)
	if length(list) = 0 then
		return item
	end if
	if find(item, list) = 0 then
		if default>=1 and default<=length(list) then
		    return list[default]
		else
			return list[1]
		end if
	end if
	return item
end function

--****
-- === Roundings and Remainders

--**
-- computes the remainder of the division of two objects using floored division.
public function mod(object x, object y)
	if equal(sign(x), sign(y)) then
		return remainder(x,y)
	end if
	return x - y * floor(x / y)
end function

--**
-- returns the integer portion of a number.
public function trunc(object x)
	return sign(x) * floor(abs(x))
end function

--**
-- returns the fractional portion of a number.
public function frac(object x)
	object temp = abs(x)
	return sign(x) * (temp - floor(temp))
end function

--**
-- returns an integral division of two objects.
public function intdiv(object a, object b)
	return sign(a)*ceil(abs(a)/abs(b))
end function

--**
-- computes the next integer equal or greater than the argument.
public function ceil(object a)
	return -floor(-a)
end function

--**
-- returns the argument's elements rounded to some precision.
public function round(object a, object precision=1)
	integer len
	sequence s
	object t, u

	precision = abs(precision)
	if atom(a) then
		if atom(precision) then
			return floor(0.5 + (a * precision )) / precision
		end if
		len = length(precision)
		s = repeat(0, len)
		for i = 1 to len do
			t = precision[i]
			if atom (t) then
				s[i] = floor( 0.5 + (a * t)) / t
			else
				s[i] = round(a, t)
			end if
		end for
		return s
	elsif atom(precision) then
		len = length(a)
		s = repeat(0, len)
		for i = 1 to len do
			t = a[i]
			if atom(t) then
				s[i] = floor(0.5 + (t * precision)) / precision
			else
				s[i] = round(t, precision)
			end if
		end for
		return s
	end if
	len = length(a)
	if len != length(precision) then
		error:crash("The lengths of the two supplied sequences do not match.")
	end if
	s = repeat(0, len)
	for i = 1 to len do
		t = precision[i]
		if atom(t) then
			u = a[i]
			if atom(u) then
				s[i] = floor(0.5 + (u * t)) / t
			else
				s[i] = round(u, t)
			end if
		else
			s[i] = round(a[i], t)
		end if
	end for
	return s
end function

--****
-- === Trigonometry

--**
-- returns an angle given its cosine.
public function arccos(trig_range x)
--  returns angle in radians
	return mathcons:HALFPI - 2 * arctan(x / (1.0 + sqrt(1.0 - x * x)))
end function

--**
-- returns an angle given its sine.
public function arcsin(trig_range x)
--  returns angle in radians
	return 2 * arctan(x / (1.0 + sqrt(1.0 - x * x)))
end function

--**
-- calculate the arctangent of a ratio.
public function atan2(atom y, atom x)
	if x > 0 then
		return arctan(y/x)
	elsif x < 0 then
		if y < 0 then
			return arctan(y/x) - mathcons:PI
		else
			return arctan(y/x) + mathcons:PI
		end if
	elsif y > 0 then
		return mathcons:HALFPI
	elsif y < 0 then
		return -(mathcons:HALFPI)
	else
		return 0
	end if
end function

--**
-- converts an angle measured in radians to an angle measured in degrees.
public function rad2deg (object x)
   return x * mathcons:RADIANS_TO_DEGREES
end function

--**
-- converts an angle measured in degrees to an angle measured in radians.
public function deg2rad (object x)
   return x * mathcons:DEGREES_TO_RADIANS
end function

--****
-- === Logarithms and Powers

--**
-- returns the base 10 logarithm of a number.
public function log10(object x1)
	return log(x1) * mathcons:INVLN10
end function

--**
-- computes some power of E.
public function exp(atom x)
	return power( mathcons:E, x)
end function

--**
-- computes the nth Fibonacci Number.
public function fib(integer i)
	return floor((power( mathcons:PHI, i) / mathcons:SQRT5) + 0.5)
end function

--****
-- === Hyperbolic Trigonometry

--**
-- computes the hyperbolic cosine of an object.
public function cosh(object a)
    return (exp(a)+exp(-a))/2
end function

--**
-- computes the hyperbolic sine of an object.
public function sinh(object a)
    return (exp(a)-exp(-a))/2
end function

--**
-- computes the hyperbolic tangent of an object.
public function tanh(object a)
    return sinh(a)/cosh(a)
end function

--**
-- computes the reverse hyperbolic sine of an object.
public function arcsinh(object a)
    return log(a+sqrt(1+a*a))
end function

type not_below_1(object x)
    if atom(x) then
        return x>=1.0
    end if
    for i=1 to length(x) do
        if not not_below_1(x[i]) then
            return 0
        end if
    end for
    return 1
end type

--**
-- computes the reverse hyperbolic cosine of an object.
public function arccosh(not_below_1 a)
    return log(a+sqrt(a*a-1))
end function

type abs_below_1(object x)
    if atom(x) then
        return x>-1.0 and x<1.0
    end if
    for i=1 to length(x) do
        if not abs_below_1(x[i]) then
            return 0
        end if
    end for
    return 1
end type

--**
-- computes the reverse hyperbolic tangent of an object.
public function arctanh(abs_below_1 a)
    return log((1+a)/(1-a))/2
end function

--****
-- === Accumulation

--**
-- computes the sum of all atoms in the argument, no matter how deeply nested.
public function sum(object a)
	atom b
	if atom(a) then
		return a
	end if
	b = 0
	for i = 1 to length(a) do
		if atom(a[i]) then
			b += a[i]
		else
			b += sum(a[i])
		end if
	end for
	return b
end function

--**
-- computes the product of all the atom in the argument, no matter how deeply nested.
public function product(object a)
	atom b
	if atom(a) then
		return a
	end if
	b = 1
	for i = 1 to length(a) do
		if atom(a[i]) then
			b *= a[i]
		else
			b *= product(a[i])
		end if
	end for
	return b
end function

--**
-- or's together all atoms in the argument, no matter how deeply nested.
public function or_all	(object a)
	atom b
	if atom(a) then
		return a
	end if
	b = 0
	for i = 1 to length(a) do
		if atom(a[i]) then
			b = or_bits(b, a[i])
		else
			b = or_bits(b, or_all(a[i]))
		end if
	end for
	return b
end function

--****
-- === Bitwise Operations

--**
-- moves the bits in the input value by the specified distance.
public function shift_bits(object source_number, integer shift_distance)

	if sequence(source_number) then
		for i = 1 to length(source_number) do
			source_number[i] = shift_bits(source_number[i], shift_distance)
		end for
		return source_number
	end if
	source_number = and_bits(source_number, 0xFFFFFFFF)
	if shift_distance = 0 then
		return source_number
	end if

	if shift_distance < 0 then
		source_number *= power(2, -shift_distance)
	else
		integer lSigned = 0
		-- Check for the sign bit so we don't propagate it.
		if and_bits(source_number, 0x80000000) then
			lSigned = 1
			source_number = and_bits(source_number, 0x7FFFFFFF)
		end if
		source_number /= power(2, shift_distance)
		if lSigned and shift_distance < 32 then
			-- Put back the sign bit now shifted
			source_number = or_bits(source_number, power(2, 31-shift_distance))
		end if
	end if

	return and_bits(source_number, 0xFFFFFFFF)
end function

--**
-- rotates the bits in the input value by the specified distance.
public function rotate_bits(object source_number, integer shift_distance)
	atom lTemp
	atom lSave
	integer lRest

	if sequence(source_number) then
		for i = 1 to length(source_number) do
			source_number[i] = rotate_bits(source_number[i], shift_distance)
		end for
		return source_number
	end if

	source_number = and_bits(source_number, 0xFFFFFFFF)
	if shift_distance = 0 then
		return source_number
	end if

	if shift_distance < 0 then
		lSave = not_bits(power(2, 32 + shift_distance) - 1)
		lRest = 32 + shift_distance
	else
		lSave = power(2, shift_distance) - 1
		lRest = shift_distance - 32
	end if

	lTemp = shift_bits(and_bits(source_number, lSave), lRest)
	source_number = shift_bits(source_number, shift_distance)
	return or_bits(source_number, lTemp)
end function

--****
-- Arithmetic

--**
-- Returns the greater common divisor of two atoms
public function gcd(atom p, atom q)
	atom r

	-- Both arguments must be positive.
	if p < 0 then
		p = -p
	end if
	if q < 0 then
		q = -q
	end if

	-- Strip off any fractional part.
	p = floor(p)
	q = floor(q)

	-- Ensure that 'p' is not smaller than 'q'
	if p < q then
		r = p
		p = q
		q = r
	end if

	-- Special case.
	if q = 0 then
		return p
	end if

	-- repeat until I get a remainder less than 2.
    while r > 1 with entry do
    	-- set up next cycle using denominator and remainder from previous cycle.
		p = q
		q = r
	entry
		-- get remainder after dividing p by q
		r = remainder(p, q)
    end while

	if r = 1 then
		return 1
	else
		return q
	end if
end function

--****
-- Floating Point

--**
-- compares two (sets of) numbers based on approximate equality.
public function approx(object p, object q, atom epsilon = 0.005)

	if sequence(p) then
		if sequence(q) then
			if length(p) != length(q) then
				error:crash("approx(): Sequence arguments must be the same length")
			end if
			for i = 1 to length(p) do
				p[i] = approx(p[i], q[i])
			end for
			return p
		else
			for i = 1 to length(p) do
				p[i] = approx(p[i], q)
			end for
			return p
		end if
	elsif sequence(q) then
			for i = 1 to length(q) do
				q[i] = approx(p, q[i])
			end for
			return q
	else
		if p > (q + epsilon) then
			return 1
		end if

		if p < (q - epsilon) then
			return -1
		end if

		return 0
	end if
end function

--**
-- tests for power of 2.
public function powof2(object p)
	return not (and_bits(p, p-1))
end function

--**
-- tests if the supplied integer is a even or odd number.
public function is_even(integer test_integer)
	return (and_bits(test_integer, 1) = 0)
end function

--**
-- tests if the supplied Euphoria object is even or odd.
public function is_even_obj(object test_object)
	if atom(test_object) then
		if integer(test_object) then
			return (and_bits(test_object, 1) = 0)
		end if
		return 0
	end if
	for i = 1 to length(test_object) do
		test_object[i] = is_even_obj(test_object[i])
	end for

	return test_object
end function

