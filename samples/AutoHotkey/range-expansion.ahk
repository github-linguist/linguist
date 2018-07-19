msgbox % expand("-6,-3--1,3-5,7-11,14,15,17-20")

expand( range ) {
    p := 0
    while p := RegExMatch(range, "\s*(-?\d++)(?:\s*-\s*(-?\d++))?", f, p+1+StrLen(f))
        loop % (f2 ? f2-f1 : 0) + 1
            ret .= "," (A_Index-1) + f1
    return SubStr(ret, 2)
}
