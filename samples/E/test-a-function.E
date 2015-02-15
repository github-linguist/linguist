#!/usr/bin/env rune

? def isPalindrome(string :String) {
>   def upper := string.toUpperCase()
>   def last := upper.size() - 1
>   for i => c ? (upper[last - i] != c) in upper(0, upper.size() // 2) {
>     return false
>   }
>   return true
> }

? isPalindrome("")
# value: true

? isPalindrome("a")
# value: true

? isPalindrome("aa")
# value: true

? isPalindrome("baa")
# value: false

? isPalindrome("baab")
# value: true

? isPalindrome("ba_ab")
# value: true

? isPalindrome("ba_ ab")
# value: false

? isPalindrome("ba _ ab")
# value: true

? isPalindrome("ab"*2)
# value: false

? def x := "ab" * 2**15; null

? x.size()
# value: 65536

? def xreversed := "ba" * 2**15; null

? isPalindrome(x + xreversed)
# value: true

? (x + xreversed).size()
# value: 131072
