package std

import std::ascii
import char from std::ascii // NEEDED FOR A COMPILER BUG

// THIS MODULE IS TO BE DEPRECATED

/**
 * Represents all signed integers representable in 8bits
 * of space in the two's complement representation.
 */
public type i8 is (int x)
    where x >=-128 && x <= 127

/**
 * Represents all signed integers representable in 16bits
 * of space in the two's complement representation.
 */
public type i16 is (int x)
    where x >=-32768 && x <= 32768

/**
 * Represents all signed integers representable in 32bits
 * of space in the two's complement representation.
 */
public type i32 is (int x)
    where x >=-2147483648 && x <= 2147483647

/**
 * Represents all unsigned integers representable in 8bits
 * of space.
 */
public type u8 is (int x)
    where x >=0 && x <= 255

/**
 * Represents all unsigned integers representable in 16bits
 * of space.
 */
public type u16 is (int x)
    where x >= 0 && x <= 65535

/**
 * Represents all unsigned integers representable in 32bits
 * of space.
 */
public type u32 is (int x)
    where x >= 0 && x <= 4294967295

/**
 * Represents all possible unsigned integers.
 */
public type uint is (int x) where x >= 0

public type nat is (int x) where x >= 0

// public function toString(int item) -> string:
//     return Any.toString(item)

// convert an integer into an unsigned byte
public function to_unsigned_byte(u8 v) -> byte:
    //
    byte mask = 0b00000001
    byte r = 0b
    for i in 0..8:
        if (v % 2) == 1:
            r = r | mask
        v = v / 2
        mask = mask << 1
    return r

// Convert a signed integer into a single byte
public function to_signed_byte(i8 v) -> byte:
    //
    u8 u
    if v >= 0:
        u = (u8) v
    else:
        u = v + 256
    return to_unsigned_byte(u)


// convert a byte into a string
public function to_string(byte b) -> ascii::string:
    ascii::string r = [0; 8]
    for i in 0..8 where |r| == 8:
        if (b & 0b00000001) == 0b00000001:
            r[7-i] = '1'
        else:
            r[7-i] = '0'
        b = b >> 1
    return r

// Convert a byte into an unsigned int.  This assumes a little endian
// encoding.
public function to_uint(byte b) -> u8:
    u8 r = 0
    uint base = 1
    while b != 0b && base <= 128 && r < base:
        if (b & 0b00000001) == 0b00000001:
            r = r + base
        // NOTE: following mask needed in leu of unsigned right shift
        // operator.
        b = (b >> 1) & 0b01111111
        base = base * 2
    return r

// Convert a byte array into an unsigned int assuming a little endian
// form for both individual bytes, and the array as a whole
public function to_uint(byte[] bytes) -> uint:
    uint val = 0
    uint base = 1
    for i in 0..|bytes|:
        int v = to_uint(bytes[i]) * base
        val = val + v
        base = base * 256
    return val

// Convert a byte into an unsigned int.  This assumes a little endian
// encoding.
public function to_int(byte b) -> int:
    uint r = 0
    uint base = 1
    while b != 0b:
        if (b & 0b00000001) == 0b00000001:
            r = r + base
        // NOTE: following mask needed in leu of unsigned right shift
        // operator.
        b = (b >> 1) & 0b01111111
        base = base * 2
    // finally, add the sign
    if r >= 128:
        return -(256-r)
    else:
        return r

// Convert a byte array into a signed int assuming a little endian
// form for both individual bytes, and the array as a whole
public function to_int(byte[] bytes) -> int:
    uint val = 0
    uint base = 1
    //
    for i in 0..|bytes|:
        int v = to_uint(bytes[i]) * base
        val = val + v
        base = base * 256
    // finally, add the sign
    if val >= (base/2):
        return -(base-val)
    else:
        return val
