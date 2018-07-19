import org.scalacheck._
import Prop._
import Gen._

object PalindromeCheck extends Properties("Palindrome") {
  property("A string concatenated with its reverse is a palindrome") =
    forAll { s: String => isPalindrome(s + s.reverse) }

  property("A string concatenated with any character and its reverse is a palindrome") =
    forAll { (s: String, c: Char) => isPalindrome(s + c + s.reverse) }

  property("If the first half of a string is equal to the reverse of its second half, it is a palindrome") =
    forAll { (s: String) => s.take(s.length / 2) != s.drop((s.length + 1) / 2).reverse || isPalindrome(s) }

  property("If the first half of a string is different than the reverse of its second half, it isn't a palindrome") =
    forAll { (s: String) => s.take(s.length / 2) == s.drop((s.length + 1) / 2).reverse || !isPalindrome(s) }

}
