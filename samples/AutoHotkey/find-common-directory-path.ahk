Dir1 := "/home/user1/tmp/coverage/test"
Dir2 := "/home/user1/tmp/covert/operator"
Dir3 := "/home/user1/tmp/coven/members"

StringSplit, Dir1_, Dir1, /
StringSplit, Dir2_, Dir2, /
StringSplit, Dir3_, Dir3, /

Loop
    If  (Dir1_%A_Index% = Dir2_%A_Index%)
    And (Dir1_%A_Index% = Dir3_%A_Index%)
        Result .= (A_Index=1 ? "" : "/") Dir1_%A_Index%
    Else Break

MsgBox, % Result
