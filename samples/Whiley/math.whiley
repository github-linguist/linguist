package std

/**
 * Return absolute value of integer variable.
 */
public function abs(int x) -> (int r)
// if input positive, then result equals input
ensures x >= 0 ==> r == x
// if input negative, then result equals negated input
ensures x < 0 ==> r == -x:
    //
    if x < 0:
        return -x
    else:
        return x

/**
 * Return maximum of two integer variables
 */
public function max(int a, int b) -> (int r)
// Return cannot be smaller than either parameter
ensures r >= a && r >= b
// Return value must equal one parameter
ensures r == a || r == b:
    //
    if a < b:
        return b
    else:
        return a

/**
 * Return minimum of two integer variables
 */
public function min(int a, int b) -> (int r)
// Return cannot be greater than either parameter
ensures r <= a && r <= b
// Return value must equal one parameter
ensures r == a || r == b:
    //
    if a > b:
        return b
    else:
        return a

/**
 * Return integer value raised to a given power.
 */
public function pow(int base, int exponent) -> int
// Exponent cannot be negative
requires exponent > 0:
    //
    int r = 1
    for i in 0..exponent:
        r = r * base
    return r

// Based on an excellent article entitled "Integer Square Roots"
// by Jack W. Crenshaw, published in the eetimes, 1998.
public function isqrt(int x) -> (int r)
requires x >= 0
ensures r >= 0:
    //
    int square = 1
    int delta = 3
    while square <= x where delta >= 3:
        square = square + delta
        delta = delta + 2
    return (delta/2) - 1
