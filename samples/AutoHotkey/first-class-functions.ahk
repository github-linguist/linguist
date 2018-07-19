forward := "sin,cube,cos"
inverse := "Asin,cuberoot,Acos"
StringSplit, forward, forward, `,  ; store array length in forward0
StringSplit, inverse, inverse, `,  ; array contents are in inverse1, inverse2...
Loop, % forward0
  MsgBox % map(compose(forward%A_Index%, inverse%A_Index%), 0.500)
Return

compose(f, g){
  Return map(0, 0, f, g)
}

map(ab = 0, x = 0 , a = 0, b = 0)
{
  Static
  If (a And b)
    Return a . "`n" . b
  If ab
  {
    StringSplit, ab, ab, `n
    Return %ab1%(%ab2%(x))
  }
}

cube(x){
  Return x ** 3
}

cuberoot(x){
  Return x ** (1 / 3)
}
