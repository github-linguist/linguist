msgbox % Clipboard := I(6)
return

I(n){
    r := "--`n" , s := " "
    loop % n
    {
        k := A_index , r .= "|  "
        loop % n
            r .= A_index=k ? "1, " : "0, "
        r := RTrim(r, " ,") , r .= "  |`n"
    }
    loop % 4*n
        s .= " "
    return Rtrim(r,"`n") "`n" s "--"
}
