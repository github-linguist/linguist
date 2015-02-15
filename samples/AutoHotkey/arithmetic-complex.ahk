Cset(C,1,1)
MsgBox % Cstr(C)  ; 1 + i*1
Cneg(C,C)
MsgBox % Cstr(C)  ; -1 - i*1
Cadd(C,C,C)
MsgBox % Cstr(C)  ; -2 - i*2
Cinv(D,C)
MsgBox % Cstr(D)  ; -0.25 + 0.25*i
Cmul(C,C,D)
MsgBox % Cstr(C)  ; 1 + i*0

Cset(ByRef C, re, im) {
   VarSetCapacity(C,16)
   NumPut(re,C,0,"double")
   NumPut(im,C,8,"double")
}
Cre(ByRef C) {
   Return NumGet(C,0,"double")
}
Cim(ByRef C) {
   Return NumGet(C,8,"double")
}
Cstr(ByRef C) {
   Return Cre(C) ((i:=Cim(C))<0 ? " - i*" . -i : " + i*" . i)
}
Cadd(ByRef C, ByRef A, ByRef B) {
   VarSetCapacity(C,16)
   NumPut(Cre(A)+Cre(B),C,0,"double")
   NumPut(Cim(A)+Cim(B),C,8,"double")
}
Cmul(ByRef C, ByRef A, ByRef B) {
   VarSetCapacity(C,16)
   t := Cre(A)*Cim(B)+Cim(A)*Cre(B)
   NumPut(Cre(A)*Cre(B)-Cim(A)*Cim(B),C,0,"double")
   NumPut(t,C,8,"double") ; A or B can be C!
}
Cneg(ByRef C, ByRef A) {
   VarSetCapacity(C,16)
   NumPut(-Cre(A),C,0,"double")
   NumPut(-Cim(A),C,8,"double")
}
Cinv(ByRef C, ByRef A) {
   VarSetCapacity(C,16)
   d := Cre(A)**2 + Cim(A)**2
   NumPut( Cre(A)/d,C,0,"double")
   NumPut(-Cim(A)/d,C,8,"double")
}
