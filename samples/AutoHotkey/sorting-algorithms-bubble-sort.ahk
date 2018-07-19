var =
(
dog
cat
pile
abc
)
MsgBox % bubblesort(var)

bubblesort(var) ; each line of var is an element of the array
{
  StringSplit, array, var, `n
  hasChanged = 1
  size := array0
  While hasChanged
  {
    hasChanged = 0
    Loop, % (size - 1)
    {
      i := array%A_Index%
      aj := A_Index + 1
      j := array%aj%
      If (j < i)
      {
        temp := array%A_Index%
        array%A_Index% := array%aj%
        array%aj% := temp
        hasChanged = 1
      }
    }
  }
  Loop, % size
    sorted .= array%A_Index% . "`n"
  Return sorted
}
