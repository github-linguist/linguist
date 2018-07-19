package require tcltest 2
source palindrome.tcl; # Assume that this is what loads the implementation of ‘palindrome’

tcltest::test palindrome-1 {check for palindromicity} -body {
    palindrome abcdedcba
} -result 1
tcltest::test palindrome-2 {check for non-palindromicity} -body {
    palindrome abcdef
} -result 0
tcltest::test palindrome-3 {check for palindrome error} -body {
    palindrome
} -returnCodes error -result "wrong # args: should be \"palindrome s\""

tcltest::cleanupTests
