package std

public type uint is (int x) where x >= 0

// Uniquely identifies a UTF code point.  Since a utf8 string does not
// store characters internally, they must be extracted on demand.
public type char is (int c) where 0 <= c && c <= 1112064

// Identifies where a given byte is internal to code point or not.
// Specifically, whether or not it conforms to the TRAILING_BYTE_MASK
public property is_internal(byte data) -> (bool r):
    (data & TRAILING_BYTE_MASK) == data

// Identifies whether a given byte is the start of a one-byte code
// point.  That is, whether or not it conforms to the ONE_BYTE_MASK.
public property is_start_one(byte data) -> (bool r):
    (data & ONE_BYTE_MASK) == data

// Identifies whether a given byte is the start of a two-byte code
// point.  That is, whether or not it conforms to the TWO_BYTE_MASK.
public property is_start_two(byte data) -> (bool r):
    (data & TWO_BYTE_MASK) == data

// Identifies whether a given byte is the start of a three-byte code
// point.  That is, whether or not it conforms to the THREE_BYTE_MASK.
public property is_start_three(byte data) -> (bool r):
    (data & THREE_BYTE_MASK) == data

// Identifies whether a given byte is the start of a four-byte code
// point.  That is, whether or not it conforms to the FOUR_BYTE_MASK.
public property is_start_four(byte data) -> (bool r):
    (data & THREE_BYTE_MASK) == data

// Identifies whether a given byte is the start of a code byte
// containing *at least* n bytes.
public property is_start_n(byte data, uint len) -> (bool r):
    (is_start_one(data) && len == 1) ||
    (is_start_two(data) && len >= 2) ||
    (is_start_three(data) && len >= 3) ||
    (is_start_four(data) && len >= 4)

public property valid_2nd_byte(byte[] chars, uint i) -> (bool r):
    (i > 0 && is_internal(chars[i])) ==> is_start_n(chars[i-1],2)

public property valid_3rd_byte(byte[] chars, uint i) -> (bool r):
    (i > 1 && is_internal(chars[i])) ==> is_start_n(chars[i-2],3)

public property valid_4th_byte(byte[] chars, uint i) -> (bool r):
    (i > 2 && is_internal(chars[i])) ==> is_start_n(chars[i-2],4)

// A UTF-8 string is a sequence of zero or more bytes with additional
// constraints.  For example, the final byte cannot conform to the
// TWO_BYTE_MASK.
public type string is (byte[] chars)
// Ensure all internal bytes in second position are valid
where all { i in 0..|chars| | valid_2nd_byte(chars, (uint) i) }
// Ensure all internal bytes in third position are valid
where all { i in 0..|chars| | valid_3rd_byte(chars, (uint) i) }
// Ensure all internal bytes in fourth position are valid
where all { i in 0..|chars| | valid_4th_byte(chars, (uint) i) }

// Identifies a UTF-8 code point that occupies one byte.  These
// characters corresponding directly to the ASCII character set.
public final byte ONE_BYTE_MASK = 0b01111111

// Identifies a UTF-8 code point that occupies two bytes, where the
// following byte conforms to the trailing byte mask.
public final byte TWO_BYTE_MASK = 0b11011111

// Identifies a UTF-8 code point that occupies three bytes, where the
// following bytes conform to the trailing byte mask.
public final byte THREE_BYTE_MASK = 0b11101111

// Identifies a UTF-8 code point that occupies four bytes, where the
// following bytes conform to the trailing byte mask.
public final byte FOUR_BYTE_MASK = 0b11110111

// Identifies an internal byte for a UTF-8 code point.  Unfortunately,
// the mask does not tell us whether we are part of a two-, three- or
// four-byte code point.
public final byte TRAILING_BYTE_MASK = 0b10111111

// Returns the length of a given utf8 string.  That is, the number of
// code points defined in the string.  This may be less than the
// number of bytes making up the string since it may contain code points
// that occupy two-, three-, or four- bytes.  This function takes time
// linear in the number bytes making up the string.
public function length(string str) -> (uint x)
// The length of a UTF-8 string may be less than the number of its
// bytes, since it may contain code points that occupy multiple bytes.
ensures x <= |str|:
    //
    uint len = 0
    //
    for i in 0..|str| where len <= i:
        byte data = str[i]
        // Check whether have internal byte or not.
        if (data & TRAILING_BYTE_MASK) != data:
            // Not internal byte, hence identifies code point.
            len = len + 1
    //
    return len

