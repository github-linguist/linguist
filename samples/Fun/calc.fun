
########################################
# calc
########################################
#   Number: + - * / div mod ^
#   Bits:   << >> (bit not/and/or/xor)
#   String: &
#   Time:   + -
#   Bool:   not and or xor
#   Regex:  =~ !~
#   Set:    in
#   Others: = !=(<>) < <= > >=
#     Set/Others for any types
########################################
# precedence (default left association)
########################################
#   () [] .
#   ^
#   * / div mod
#   (bit not)                   -> right
#   (bit and/or/xor) << >>
#   + -
#   &
#   = != <> < <= > >= in =~ !~  -> none
#   not                         -> right
#   and
#   or xor
########################################
# short-circuit calculation
########################################
#   and
#   or
########################################

# Number
var n1 = 9;
var n2 = 3.3;
?. n1 + n2;
?. n1 - n2;
?. n1 * n2;
?. n1 / n2;
?. n1 div "4"; # Type conversions automatically
?. n1 mod '4'; # Type conversions automatically
?. n1 ^ 3;     # power

# Bits
n1 = 0x0f;
n2 = 0xf0;
?. n1 << 4;    # shift left
?. n2 >> 4;    # shift right
?. bit not n1;
?. n1 bit and n2;
?. n1 bit or  n2;
?. n1 bit xor n2;

# String
var s1 = 'Hello';
var s2 = ", world!";
?. s1 & s2 & ' - ' & 10000; # Type conversions automatically

# Time
var t1 = @"2010-04-09 15:35:15";
var t2 = @'2010-03-09 15:35:15';
?. t1 + 100;
?. t1 - t2;

# Bool
var b1 = true;
var b2 = false;
?. not not b1;
?. not (b1 and b2);
?. b1 or  b2;
?. b1 xor b2;

# Set
?. "Hello" in ["Hello", "World"];

# Others - Compare
?. not 1 = 2;
?. 1 != 2;
?. 1 <> 2;
?. 1 < 2;
?. 1 <= 2;
?. not 1 > 2;
?. not 1 >= 2;

# Null - All null values equal null (number, string, bool, etc.)
?. null = 0;
?. null = false;
?. null = '';

# Short-circuit calculation
?. true  or  1 = 0; # "1 = 0" doesn't need calc
?. false and 1 = 1; # "1 = 1" doesn't need calc

# Optional
?. Undefined?;
?. Undefined?.Undefined?;
