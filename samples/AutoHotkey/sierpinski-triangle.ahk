Loop 6
   MsgBox % Triangle(A_Index)

Triangle(n,x=0,y=1) { ; Triangle(n) -> string of dots and spaces of Sierpinski triangle
   Static t, l                                  ; put chars in a static string
   If (x < 1) {                                 ; when called with one parameter
      l := 2*x := 1<<(n-1)                      ; - compute location, string size
      VarSetCapacity(t,l*x,32)                  ; - allocate memory filled with spaces
      Loop %x%
         NumPut(13,t,A_Index*l-1,"char")        ; - new lines in the end of rows
   }
   If (n = 1)                                   ; at the bottom of recursion
      Return t, NumPut(46,t,x-1+(y-1)*l,"char") ; - write "." (better at proportional fonts)
   u := 1<<(n-2)
   Triangle(n-1,x,y)                            ; draw smaller triangle here
   Triangle(n-1,x-u,y+u)                        ; smaller triangle down-left
   Triangle(n-1,x+u,y+u)                        ; smaller triangle down right
   Return t
}
