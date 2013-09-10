;------------------------------------
; These first two routines illustrate
; the dynamic scope of variables in M
;------------------------------------
triangle1(x) ;;
 set sum=0
 for  do  quit:x'>1
 . set sum=sum+x
 . set x=x-1
 quit sum

main1() ;;
 set sum=1500
 set x=6
 write "sum before=",sum,!
 set y=$$triangle1(x)
 write "sum after=",sum,!
 write "triangle of ",x," is ",y,!
 quit


;------------------------------------
; These next two routines illustrate
; the use of the NEW command to make
; variables limited to the local scope
;------------------------------------
triangle2(x) ;;
 new sum ; <-- HERE !!
 set sum=0
 for  do  quit:x'>1
 . set sum=sum+x
 . set x=x-1
 quit sum

main2() ;;
 set sum=1500
 set x=6
 write "sum before=",sum,!
 set y=$$triangle2(x)
 write "sum after=",sum,!
 write "triangle of ",x," is ",y,!
 quit
