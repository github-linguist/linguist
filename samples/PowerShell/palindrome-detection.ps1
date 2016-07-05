Function Test-Palindrome( [String] $Text ){
    $CharArray = $Text.ToCharArray()
    [Array]::Reverse($CharArray)
    $Text.ToCharArray() -eq $CharArray
}
