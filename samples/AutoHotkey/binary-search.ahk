array := "1,2,4,6,8,9"
StringSplit, A, array, `,   ; creates associative array
MsgBox % x := BinarySearch(A, 4, 1, A0) ; Recursive
MsgBox % A%x%
MsgBox % x := BinarySearchI(A, A0, 4)  ; Iterative
MsgBox % A%x%

BinarySearch(A, value, low, high) { ; A0 contains length of array
  If (high < low)               ; A1, A2, A3...An are array elements
    Return not_found
  mid := Floor((low + high) / 2)
  If (A%mid% > value) ; A%mid% is automatically global since no such locals are present
    Return BinarySearch(A, value, low, mid - 1)
  Else If (A%mid% < value)
    Return BinarySearch(A, value, mid + 1, high)
  Else
    Return mid
}

BinarySearchI(A, lengthA, value) {
  low := 0
  high := lengthA - 1
  While (low <= high) {
    mid := Floor((low + high) / 2) ; round to lower integer
    If (A%mid% > value)
      high := mid - 1
    Else If (A%mid% < value)
      low := mid + 1
    Else
      Return mid
  }
  Return not_found
}
