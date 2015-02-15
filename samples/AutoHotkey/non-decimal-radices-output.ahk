MsgBox % BC("FF",16,3) ; -> 100110 in base 3 = FF in hex = 256 in base 10

BC(NumStr,InputBase=8,OutputBase=10) {
  Static S = 12345678901234567890123456789012345678901234567890123456789012345
  DllCall("msvcrt\_i64toa","Int64",DllCall("msvcrt\_strtoui64","Str",NumStr,"Uint",0,"UInt",InputBase,"CDECLInt64"),"Str",S,"UInt",OutputBase,"CDECL")
  Return S
}
