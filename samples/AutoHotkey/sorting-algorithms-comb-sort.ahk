List1 = 23,76,99,58,97,57,35,89,51,38,95,92,24,46,31,24,14,12,57,78
List2 = 88,18,31,44,4,0,8,81,14,78,20,76,84,33,73,75,82,5,62,70

List2Array(List1, "MyArray")
CombSort("MyArray")
MsgBox, % List1 "`n" Array2List("MyArray")

List2Array(List2, "MyArray")
CombSort("MyArray")
MsgBox, % List2 "`n" Array2List("MyArray")



;---------------------------------------------------------------------------
CombSort(Array) { ; CombSort of Array %Array%, length = %Array%0
;---------------------------------------------------------------------------
    Gap := %Array%0
    While Gap > 1 Or Swaps {
        If (Gap > 1)
            Gap := 4 * Gap // 5
        i := Swaps := False
        While (j := ++i + Gap) <= %Array%0 {
            If (%Array%%i% > %Array%%j%) {
                Swaps := True
                %Array%%i% := (%Array%%j% "", %Array%%j% := %Array%%i%)
            }
        }
    }
}



;---------------------------------------------------------------------------
List2Array(List, Array) { ; creates an array from a comma separated list
;---------------------------------------------------------------------------
    global
    StringSplit, %Array%, List, `,
}



;---------------------------------------------------------------------------
Array2List(Array) { ; returns a comma separated list from an array
;---------------------------------------------------------------------------
    Loop, % %Array%0
        List .= (A_Index = 1 ? "" : ",") %Array%%A_Index%
    Return, List
}
