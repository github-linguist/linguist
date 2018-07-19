msgbox % extract("0,1,2,4,6,7,8,11,12,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,35,36,37,38,39")

extract( list ) {
    loop, parse, list, `,, %A_Tab%%A_Space%`r`n
    {
        if (A_LoopField+0 != p+1)
            ret .= (f!=p ? (p>f+1 ? "-" : ",") p : "") "," f := A_LoopField
        p := A_LoopField
    }
    return SubStr(ret (f!=p ? (p>f+1 ? "-" : ",") p : ""), 2)
}
