fun {Balance Col A X B}
   case Col#A#X#B
   of b#t(r t(r A X B) Y C         )#Z#D                            then t(r t(b A X B) Y t(b C Z D))
   [] b#t(r A          X t(r B Y C))#Z#D                            then t(r t(b A X B) Y t(b C Z D))
   [] b#A                           #X#t(r t(r B Y C) Z D)          then t(r t(b A X B) Y t(b C Z D))
   [] b#A                           #X#t(r B          Y t(r C Z D)) then t(r t(b A X B) Y t(b C Z D))
   else t(Col A X B)
   end
end

fun {Insert X S}
   fun {Ins S}
      case S of e then t(r e X e)
      [] t(Col A Y B) then
	 if X < Y then {Balance Col {Ins A} Y B}
	 elseif X > Y then {Balance Col A Y {Ins B}}
	 else S
	 end
      end
   end
   t(_ A Y B) = {Ins S}
in
   t(b A Y B)
end
