def isPalindrome(string :String) {
  def upper := string.toUpperCase()
  def last := upper.size() - 1
  for i => c ? (upper[last - i] != c) in upper(0, upper.size() // 2) {
    return false
  }
  return true
}
