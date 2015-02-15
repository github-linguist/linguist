class TestPalindrome : Test
{
  public Void testIsPalindrome ()
  {
    verify(Palindrome.isPalindrome(""))
    verify(Palindrome.isPalindrome("a"))
    verify(Palindrome.isPalindrome("aa"))
    verify(Palindrome.isPalindrome("aba"))
    verifyFalse(Palindrome.isPalindrome("abb"))
    verify(Palindrome.isPalindrome("salàlas"))
    verify(Palindrome.isPalindrome("In girum imus nocte et consumimur igni".lower.replace(" ","")))
  }
}
