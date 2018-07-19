comprehend("show", range(1, 20), "triples")
return

comprehend(doToVariable, inSet, satisfying)
{
  set := %satisfying%(inSet.begin, inSet.end)
  index := 1
  While % set[index, 1]
  {
    item := set[index, 1] . ", " . set[index, 2] . ", " . set[index, 3]
    %doToVariable%(item)
    index += 1
  }
  return
}


show(var)
{
  msgbox % var
}

range(begin, end)
 {
   set := object()
   set.begin := begin
   set.end := end
   return set
 }

!r::reload
!q::exitapp

triples(begin, end)
{

  set := object()
  index := 1
  range := end - begin

  loop, % range
  {
    x := begin + A_Index
    loop, % range
    {
      y := A_Index + x
      if y > 20
	break
loop, % range
{
  z := A_Index + y
  if z > 20
    break
  isTriple := ((x ** 2 + y ** 2) == z ** 2)
  if isTriple
  {
    set[index, 1] := x
    set[index, 2] := y
    set[index, 3] := z
    index += 1
  ; msgbox % "triple: "  x . y . z
  }

}
}
}
return set
}
