list := object(1, object(1, 1), 2, 2, 3, object(1, object(1, 3, 2, 4)
, 2, 5), 4, object(1, object(1, object(1, object()))), 5
, object(1, object(1, 6)), 6, 7, 7, 8, 9, object())
msgbox % objPrint(list) ; (( 1 ) 2 (( 3  4 ) 5 )(((())))(( 6 )) 7  8 ())
msgbox % objPrint(objFlatten(list)) ; ( 1  2  3  4  5  6  7  8 )
return

!r::reload
!q::exitapp

objPrint(ast, reserved=0)
{
  if !isobject(ast)
    return " " ast " "

  if !reserved
    reserved := object("seen" . &ast, 1)  ; to keep track of unique objects within top object

  enum := ast._newenum()
  while enum[key, value]
  {
    if reserved["seen" . &value]
      continue  ; don't copy repeat objects (circular references)
;   string .= key . ": " . objPrint(value, reserved)
    string .= objPrint(value, reserved)
  }
  return "(" string ")"
}


objFlatten(ast)
{
  if !isobject(ast)
    return ast

  flat := object() ; flat object

  enum := ast._newenum()
  while enum[key, value]
  {
    if !isobject(value)
      flat._Insert(value)
    else
    {
      next := objFlatten(value)
      loop % next._MaxIndex()
      flat._Insert(next[A_Index])

    }
  }
  return flat
}
