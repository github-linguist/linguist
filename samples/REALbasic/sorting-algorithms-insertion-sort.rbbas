Sub InsertionSort(theList() as Integer)
  for insertionElementIndex as Integer = 1 to UBound(theList)
    dim insertionElement as Integer = theList(insertionElementIndex)
    dim j as Integer = insertionElementIndex - 1
    while (j >= 0) and (insertionElement < theList(j))
      theList(j + 1) = theList(j)
      j = j - 1
    wend
    theList(j + 1) = insertionElement
  next
End Sub
