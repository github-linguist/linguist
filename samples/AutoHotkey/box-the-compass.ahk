get_Index(angle){
    return Mod(floor(angle / 11.25 +0.5), 32) + 1
}

get_Abbr_From_Index(i){
    static points
       := [ "N", "NbE", "NNE", "NEbN", "NE", "NEbE", "ENE", "EbN"
           ,"E", "EbS", "ESE", "SEbE", "SE", "SEbS", "SSE", "SbE"
           ,"S", "SbW", "SSW", "SWbS", "SW", "SWbW", "WSW", "WbS"
           ,"W", "WbN", "WNW", "NWbW", "NW", "NWbN", "NNW", "NbW" ]
    return points[i]
}

Build_Name_From_Abbr(a){
    Loop Parse, a
    {
        i := A_Index
        if ((i = 2) && (SubStr(a, i, 1) != "b") && (StrLen(a) == 3))
            retval .= "-"
        retval .= {N: "north", S: "south", E: "east"
                 , W: "west" , b: " by "}[A_LoopField]
    }
    return Chr(Asc(SubStr(retval, 1, 1))-32) . SubStr(retval, 2)
}

; test

headings:= [0.00, 16.87, 16.88, 33.75, 50.62, 50.63, 67.50, 84.37, 84.38, 101.25
          , 118.12, 118.13, 135.00, 151.87, 151.88, 168.75, 185.62, 185.63
          , 202.50, 219.37, 219.38, 236.25, 253.12, 253.13, 270.00, 286.87
          , 286.88, 303.75, 320.62, 320.63, 337.50, 354.37, 354.38]
For n, a in headings
{
    i := get_Index(a)
    out .= SubStr(" " i, -1) " "
        . SubStr(Build_Name_From_Abbr(get_Abbr_From_Index(i))
        . "                    ", 1, 24) . SubStr("  " a, -5)  . "`r`n" ;
}
clipboard := out
