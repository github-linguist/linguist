# test string
$s = "abcdefgh"
# test parameters
$n, $m, $c, $s2 = 2, 3, [char]'d', $s2 = 'cd'

# starting from n characters in and of m length
# n = 2, m = 3
$s.Substring($n-1, $m)              # returns 'bcd'

# starting from n characters in, up to the end of the string
# n = 2
$s.Substring($n-1)                  # returns 'bcdefgh'

# whole string minus last character
$s.Substring(0, $s.Length - 1)      # returns 'abcdefg'

# starting from a known character within the string and of m length
# c = 'd', m =3
$s.Substring($s.IndexOf($c), $m)    # returns 'def'

# starting from a known substring within the string and of m length
# s2 = 'cd', m = 3
$s.Substring($s.IndexOf($s2), $m)   # returns 'cde'
