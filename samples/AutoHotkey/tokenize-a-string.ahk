string := "Hello,How,Are,You,Today"
stringsplit, string, string, `,
loop, % string0
{
msgbox % string%A_Index%
}
