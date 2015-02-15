Define.f meanTreated,meanControl,diffInMeans
Define.f actualmeanTreated,actualmeanControl,actualdiffInMeans

Dim poolA(19)

poolA(1) =85 ; first 9 the treated
poolA(2) =88
poolA(3) =75
poolA(4) =66
poolA(5) =25
poolA(6) =29
poolA(7) =83
poolA(8) =39
poolA(9) =97

poolA(10) =68 ; last 10 the control
poolA(11) =41
poolA(12) =10
poolA(13) =49
poolA(14) =16
poolA(15) =65
poolA(16) =32
poolA(17) =92
poolA(18) =28
poolA(19) =98

Procedure.i IsValidBitString(x,pool,treated)
Protected c,i
For i=1 to pool
mask=1<<(i-1)
If mask&x:c+1:EndIf
Next
If c=treated :ProcedureReturn x
Else         :ProcedureReturn 0
EndIf
EndProcedure

treated=9
control=10

pool   =treated+control

; actual Experimentally observed difference in means

For i=1 to Treated
sumTreated+poolA(i)
Next
For i=Treated+1 to Treated+Control
sumControl+poolA(i)
Next

actualmeanTreated=sumTreated /Treated
actualmeanControl=sumControl /Control
actualdiffInMeans=actualmeanTreated-actualmeanControl

; exhaust the possibilites
For x=1 to 1<<pool

; Valid? i.e. are there 9 "1's" ?
If IsValidBitString(x,pool,treated)
TotalComBinations+1:sumTreated=0:sumControl=0

; separate the groups
For i=pool to 1 Step -1
mask=1<<(i-1):idx=pool-i+1
If mask&x
sumTreated+poolA(idx)
Else
sumControl+poolA(idx)
EndIf
Next

meanTreated=sumTreated /Treated
meanControl=sumControl /Control
diffInMeans=meanTreated-meanControl
; gather the statistics
If (diffInMeans)<=(actualdiffInMeans)
diffLessOrEqual+1
Else
diffGreater+1
EndIf

EndIf
Next
; show our results
; cw(StrF(100*diffLessOrEqual/TotalComBinations,2)+" "+Str(diffLessOrEqual))
; cw(StrF(100*diffGreater    /TotalComBinations,2)+" "+Str(diffGreater))

Debug StrF(100*diffLessOrEqual/TotalComBinations,2)+" "+Str(diffLessOrEqual)
Debug StrF(100*diffGreater    /TotalComBinations,2)+" "+Str(diffGreater)
