Program Transpose;

const
  A: array[1..3,1..5] of integer = (( 1,  2,  3,  4,  5),
                                    ( 6,  7,  8,  9, 10),
				    (11, 12, 13, 14, 15)
				   );
var
  B: array[1..5,1..3] of integer;
  i, j: integer;

begin
  for i := low(A) to high(A) do
    for j := low(A[1]) to high(A[1]) do
      B[j,i] := A[i,j];

  writeln ('A:');
  for i := low(A) to high(A) do
  begin
    for j := low(A[1]) to high(A[1]) do
      write (A[i,j]:3);
    writeln;
  end;

  writeln ('B:');
  for i := low(B) to high(B) do
  begin
    for j := low(B[1]) to high(B[1]) do
      write (B[i,j]:3);
    writeln;
  end;
end.
