std(2),std(4),std(4),std(4),std(5),std(5),std(7)
MsgBox % std(9) ; 2

std(x="") {
  Static sum:=0, sqr:=0, n:=0
  If (x="")                    ; blank parameter: reset
     sum := 0, sqr := 0, n := 0
  Else
     sum += x, sqr += x*x, n++ ; update state
  Return sqrt((sqr-sum*sum/n)/n)
}
