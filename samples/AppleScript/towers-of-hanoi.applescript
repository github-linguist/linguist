global moves --this is so the handler 'hanoi' can see the 'moves' variable
set moves to ""
hanoi(4, "peg A", "peg C", "peg B")

on hanoi(ndisks, fromPeg, toPeg, withPeg)
    if ndisks is greater than 0 then
        hanoi(ndisks - 1, fromPeg, withPeg, toPeg)
        set moves to moves & "Move disk " & ndisks & " from " & fromPeg & " to " & toPeg & return
        hanoi(ndisks - 1, withPeg, toPeg, fromPeg)
    end if
    return moves
end hanoi
