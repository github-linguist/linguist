Procedure Bubble_sort(Array idx(1), n, Array buf(1))
  Protected i, j
  SortArray(idx(),#PB_Sort_Ascending)
  For i=0 To n
    For j=i+1 To n
      If buf(idx(j)) < buf(idx(i))
        Swap buf(idx(j)), buf(idx(i))
      EndIf
    Next
  Next
EndProcedure

Procedure main()
  DataSection
    values: Data.i 7, 6, 5, 4, 3, 2, 1, 0
    indices:Data.i 6, 1, 7
  EndDataSection

  Dim values.i(7) :CopyMemory(?values, @values(), SizeOf(Integer)*8)
  Dim indices.i(2):CopyMemory(?indices,@indices(),SizeOf(Integer)*3)

  If OpenConsole()
    Protected i
    PrintN("Before sort:")
    For i=0 To ArraySize(values())
      Print(Str(values(i))+" ")
    Next

    PrintN(#CRLF$+#CRLF$+"After sort:")
    Bubble_sort(indices(), ArraySize(indices()), values())
    For i=0 To ArraySize(values())
      Print(Str(values(i))+" ")
    Next

    Print(#CRLF$+#CRLF$+"Press ENTER to exit")
    Input()
  EndIf
EndProcedure

main()
