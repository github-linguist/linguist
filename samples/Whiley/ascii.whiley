package std

import std::integer
import std::array
import u8,uint from std::integer

// The ASCII standard (INCITS 4-1986[R2012]) defines a 7bit character
// encoding.
public type char is (int x) where 0 <= x && x <= 127

// Define the ASCII letter
public type letter is (int x) where ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

// Define the ASCII uppercase letter
public type uppercase is (int x) where ('A' <= x && x <= 'Z')

// Define the ASCII lowercase letter
public type lowercase is (int x) where ('a' <= x && x <= 'z')

// Define the ASCII digit
public type digit is (int x) where ('0' <= x && x <= '9')

// Define string as sequence of ASCII characters
public type string is char[]

// === CONTROL CHARACTERS ===

// Null character
public int NUL = 0

// Start of Header
public int SOH = 1

// Start of Text
public int STX = 2

// End of Text
public int ETX = 3

// End of Transmission
public int EOT = 4

// Enquiry
public int ENQ = 5

// Acknowledgment
public int ACK = 6

// Bell
public int BEL = 7

// Backspace
public int BS = 8

// Horizontal Tab
public int HT = 9

// Line Feed
public int LF = 10

// Vertical Tab
public int VT = 11

// Form Feed
public int FF = 12

// Carriage Return
public int CR = 13

// Shift Out
public int SO = 14

// Shift In
public int SI = 15

// Data Link Escape
public int DLE = 16

// Device Control 1
public int DC1 = 17

// Device Control 2
public int DC2 = 18

// Device Control 3
public int DC3 = 19

// Device Control 4
public int DC4 = 20

// Negative Acknowledgement
public int NAK = 21

// Synchronous Idle
public int SYN = 22

// End of Transmission Block
public int ETB = 23

// Cancel
public int CAN = 24

// End of Medium
public int EM = 25

// Substitute
public int SUB = 26

// Escape
public int ESC = 27

// File Separator
public int FS = 28

// Group Separator
public int GS = 29

// Record Separator
public int RS = 30

// Unit Separator
public int US = 31

// Delete
public int DEL = 127

// Convert an ASCII character into a byte.
public function to_byte(char v) -> byte:
    //
    byte mask = 0b00000001
    byte r = 0b
    for i in 0..8:
        if (v % 2) == 1:
            r = r | mask
        v = v / 2
        mask = mask << 1
    return r

// Convert an ASCII string into a list of bytes
public function to_bytes(string s) -> byte[]:
    byte[] r = [0b; |s|]
    for i in 0..|s| where |r| == |s|:
        r[i] = to_byte(s[i])
    return r

// Convert a list of bytes into an ASCII string
public function from_bytes(byte[] data) -> string:
    string r = [0; |data|]
    for i in 0..|data| where |r| == |data|:
        u8 v = integer::to_uint(data[i])
        if v >= 127:
            v = '?'
        r[i] = (char) v
    return r

public function is_upper_case(char c) -> bool:
    return 'A' <= c && c <= 'Z'

public function is_lower_case(char c) -> bool:
    return 'a' <= c && c <= 'z'

public function is_letter(char c) -> bool:
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

public function is_digit(char c) -> bool:
    return '0' <= c && c <= '9'

public function is_whitespace(char c) -> bool:
    return c == ' ' || c == '\t' || c == '\n' || c == '\r'

public function to_string(int item) -> string:
    //
    bool sign
    // First, normalise item and record sign
    if item < 0:
       sign = false
       item = -item
    else:
       sign = true
    // Second, determine number of digits.  This is necessary to
    // avoid unnecessary dynamic memory allocatione    
    int tmp = item
    uint digits = 0
    do:
        tmp = tmp / 10
        digits = digits + 1
    while tmp != 0 where digits > 0
    // Finally write digits into resulting string
    string r = ['0';digits]
    int i = digits
    do:
        int remainder = item % 10
        item = item / 10
        char digit = ('0' + remainder)
        i = i - 1
        r[i] = digit
    while item != 0 && i > 0 where i <= digits && |r| == digits
    //
    if sign:
        return r
    else:
        // This could be optimised!
        return array::append("-",r)

/**
 * Convert an array of integers into an ASCII String.
 */
public function to_string(int[] items) -> string:
    string r = ""
    // Convert each item in array to string
    for i in 0..|items|:
        if i != 0:
            r = array::append(r,",")
        r = array::append(r,to_string(items[i]))
    // Done
    return r    

/*
constant digits is [
    '0','1','2','3','4','5','6','7','8','9',
    'a','b','c','d','e','f','g','h'
]

// Convert an integer into a hex string
public function toHexString(int item) -> string:
    string r = ""
    int count = 0
    int i = item
    while i > 0:
        int v = i / 16
        int w = i % 16
        count = count + 1
        i = v
    //
    i = count
    while item > 0:
        i = i - 1    
        int v = item / 16
        int w = item % 16
        r[i] = digits[w]
        item = v
    //
    return r
*/

// parse a string representation of an integer value
public function parse_int(ascii::string input) -> int|null
requires |input| > 0:
    //
    // first, check for negative number
    int start = 0
    bool negative

    if input[0] == '-':
        negative = true
        start = start + 1
    else:
        negative = false
    // now, parse remaining digits
    int r = 0
    for i in start..|input|:
        char c = input[i]
        r = r * 10
        if !ascii::is_digit(c):
            return null
        r = r + ((int) c - '0')
    // done
    if negative:
        return -r
    else:
        return r
