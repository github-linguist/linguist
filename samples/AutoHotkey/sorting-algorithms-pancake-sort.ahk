;---------------------------------------------------------------------------
Loop { ; test loop
;---------------------------------------------------------------------------
    Loop, % Data0 := 10
        Random, Data%A_Index%, 10, 99
    Unsorted := Array2List("Data")
    PancakeSort("Data")
    Sorted := Array2List("Data")
    MsgBox, 1, Pancake Sort, %Unsorted%`n%Sorted%
    IfMsgBox, Cancel, Break
}



;---------------------------------------------------------------------------
PancakeSort(Array) { ; implementation of pancake sort algorithm
;---------------------------------------------------------------------------
    Loop, % %Array%0 - 1 {
        m := 0
        Loop, % s := %Array%0 - A_Index + 1
            If (m <= %Array%%A_Index%)
                m := %Array%%A_Index%, p := A_Index
        If (p < s) && (p > 1)
            Flip(Array, p)
        If (p < s)
            Flip(Array, s)
    }
}



;---------------------------------------------------------------------------
Flip(Array, n) { ; flip the first n members of Array
;---------------------------------------------------------------------------
    Loop, % x := n // 2 {
        i := n - A_Index + 1
        %Array%%i% := (%Array%%A_Index% "", %Array%%A_Index% := %Array%%i%)
    }
}



;---------------------------------------------------------------------------
Array2List(Array) { ; returns a space separated list from an array
;---------------------------------------------------------------------------
    Loop, % %Array%0
        List .= (A_Index = 1 ? "" : " ") %Array%%A_Index%
    Return, List
}
