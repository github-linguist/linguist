program IdentityMatrix(input, output);

var
  matrix: array of array of integer;
  n, i, j: integer;

begin
  write('Size of matrix: ');
  readln(n);
  setlength(matrix, n, n);

  for i := 0 to n - 1 do
    matrix[i,i] := 1;

  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      write (matrix[i,j], ' ');
    writeln;
  end;
end.
