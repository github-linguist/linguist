one_of_n(n){
    ; One based line numbers
    choice = 1
    Loop % n-1
    {
        Random, rnd, 1, % A_Index+1
        If rnd = 1
            choice := A_Index+1
    }
    return choice
}
one_of_n_test(n=10, trials=100000){
    bins := [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    Loop % trials
        bins[one_of_n(n)] += 1
    return bins
}

b := one_of_n_test()
Loop 10
   out .= A_Index ": " b[A_Index] "`n"
MsgBox % out
