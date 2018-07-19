MsgBox % CountingSort("-1,1,1,0,-1",-1,1)

CountingSort(ints,min,max) {
   Loop % max-min+1
      i := A_Index-1, a%i% := 0
   Loop Parse, ints, `, %A_Space%%A_Tab%
      i := A_LoopField-min, a%i%++
   Loop % max-min+1 {
      i := A_Index-1, v := i+min
      Loop % a%i%
         t .= "," v
   }
   Return SubStr(t,2)
}
