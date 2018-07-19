out = <table style="text-align:center; border: 1px solid"><th></th><th>X</th><th>Y</th><th>Z</th><tr>
Loop 4
    out .= "`r`n<tr><th>" A_Index "</th><td>" Rand() "</td><td>" Rand() "</td><td>" Rand() "</tr>"
out .= "`r`n</table>"
MsgBox % clipboard := out

Rand(u=1000){
    Random, n, 1, % u
    return n
}
