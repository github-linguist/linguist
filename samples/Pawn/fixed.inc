/* Fixed point arithmetic
 *
 * (c) Copyright 1998-2011, ITB CompuPhase
 * This file is provided as is (no warranties).
 */
#pragma library Fixed

const fround_method: {
  fround_round = 0,
  fround_floor,
  fround_ceil,
  fround_tozero,
  fround_unbiased
}

native Fixed:fixed(value);
native Fixed:strfixed(const string[]);
native Fixed:fmul(Fixed:oper1, Fixed:oper2);
native Fixed:fdiv(Fixed:dividend, Fixed:divisor);
native Fixed:ffract(Fixed:value);
native       fround(Fixed:value, fround_method:method=fround_round);
native Fixed:fpower(Fixed:value, exponent);
native Fixed:fsqroot(Fixed:value);
native Fixed:fabs(Fixed:value);

#pragma rational Fixed(3)

/* user defined operators */
native Fixed:operator*(Fixed:oper1, Fixed:oper2) = fmul;
native Fixed:operator/(Fixed:oper1, Fixed:oper2) = fdiv;
native Fixed:operator=(oper) = fixed;

stock Fixed:operator++(Fixed:oper)
    return oper + fixed(1);

stock Fixed:operator--(Fixed:oper)
    return oper - fixed(1);

stock Fixed:operator*(Fixed:oper1, oper2)
    return Fixed: (_:oper1 * oper2);            /* "*" is commutative */

stock Fixed:operator/(Fixed:oper1, oper2)
    return oper1 / fixed(oper2);

stock Fixed:operator/(oper1, Fixed:oper2)
    return fdiv(fixed(oper1), oper2);

stock Fixed:operator+(Fixed:oper1, oper2)
    return oper1 + fixed(oper2);                /* "+" is commutative */

stock Fixed:operator-(Fixed:oper1, oper2)
    return oper1 - fixed(oper2);

stock Fixed:operator-(oper1, Fixed:oper2)
    return fixed(oper1) - oper2;

stock bool:operator>(Fixed:oper1, oper2)
    return oper1 > fixed(oper2);

stock bool:operator>(oper1, Fixed:oper2)
    return fixed(oper1) > oper2;

stock bool:operator>=(Fixed:oper1, oper2)
    return oper1 >= fixed(oper2);

stock bool:operator>=(oper1, Fixed:oper2)
    return fixed(oper1) >= oper2;

stock bool:operator<(Fixed:oper1, oper2)
    return oper1 < fixed(oper2);

stock bool:operator<(oper1, Fixed:oper2)
    return fixed(oper1) < oper2;

stock bool:operator<=(Fixed:oper1, oper2)
    return oper1 <= fixed(oper2);

stock bool:operator<=(oper1, Fixed:oper2)
    return fixed(oper1) <= oper2;

stock bool:operator==(Fixed:oper1, oper2)       /* "==" is commutative */
    return oper1 == fixed(oper2);

stock bool:operator!=(Fixed:oper1, oper2)       /* "!=" is commutative */
    return oper1 != fixed(oper2);

/* forbidden operations */
forward operator%(Fixed:oper1, Fixed:oper2);
forward operator%(Fixed:oper1, oper2);
forward operator%(oper1, Fixed:oper2);

