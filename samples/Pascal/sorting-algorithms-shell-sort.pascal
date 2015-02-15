Const
  MaxN = 100; { number of elements (my example is 100) }
Type
  TArray = Array [0..MaxN] of Integer;

Procedure ShellSort ( var A : TArray; N : Integer );
Var
  i, j, step, tmp : Integer;
Begin
  step:=N div 2;  // step:=step shr 1
  While step>0 Do Begin
    For i:=step to N Do Begin
      tmp:=A[i];
      j:=i;
      While (j>=step) and (A[j-step]>tmp) Do Begin
        A[j]:=A[j-step];
        dec(j,step);
      End;
      A[j]:=tmp;
    End;
    step:=step div 2;  // step:=step shr 1
  End;
End;
