Program Cholesky;

type
  D2Array = array of array of double;

function cholesky(const A: D2Array): D2Array;
  var
    i, j, k: integer;
    s: double;
  begin
    setlength(cholesky, length(A), length(A));
    for i := low(cholesky) to high(cholesky) do
      for j := 0 to i do
      begin
	s := 0;
	for k := 0 to j - 1 do
	  s := s + cholesky[i][k] * cholesky[j][k];
	if i = j then
	  cholesky[i][j] := sqrt(A[i][i] - s)
	else
          cholesky[i][j] := (A[i][j] - s) / cholesky[j][j];  // save one multiplication compared to the original
      end;
  end;

procedure printM(const A: D2Array);
  var
    i, j: integer;
  begin
    for i :=  low(A) to high(A) do
    begin
      for j := low(A) to high(A) do
        write(A[i,j]:8:5);
      writeln;
    end;
  end;

const
  m1: array[0..2,0..2] of double = ((25, 15, -5),
                                    (15, 18,  0),
			            (-5,  0, 11));
  m2: array[0..3,0..3] of double = ((18, 22,  54,  42),
                                    (22, 70,  86,  62),
				    (54, 86, 174, 134),
				    (42, 62, 134, 106));
var
  index: integer;
  cIn, cOut: D2Array;

begin
  setlength(cIn, length(m1), length(m1));
  for index := low(m1) to high(m1) do
    cIn[index] := m1[index];
  cOut := cholesky(cIn);
  printM(cOut);

  writeln;

  setlength(cIn, length(m2), length(m2));
  for index := low(m2) to high(m2) do
    cIn[index] := m2[index];
  cOut := cholesky(cIn);
  printM(cOut);

end.
