{ X is array of LongInt }
Procedure QuickSort ( Left, Right : LongInt );
Var
  i, j : LongInt;
  tmp, pivot : LongInt;         { tmp & pivot are the same type as the elements of array }
Begin
  i:=Left;
  j:=Right;
  pivot := X[(Left + Right) shr 1]; // pivot := X[(Left + Rigth) div 2]
  Repeat
    While pivot > X[i] Do i:=i+1;
    While pivot < X[j] Do j:=j-1;
    If i<=j Then Begin
      tmp:=X[i];
      X[i]:=X[j];
      X[j]:=tmp;
      j:=j-1;
      i:=i+1;
    End;
  Until i>j;
  If Left<j Then QuickSort(Left,j);
  If i<Right Then QuickSort(i,Right);
End;
