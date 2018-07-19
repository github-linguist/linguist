Program GreatestSubsequentialSum(output);

var
  a: array[1..11] of integer = (-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1);
  i, j: integer;
  seqStart, seqEnd: integer;
  maxSum, seqSum: integer;

begin
  maxSum   := 0;
  seqStart := 0;
  seqEnd   := -1;
  for i := low(a) to high(a) do
  begin
    seqSum := 0;
    for j := i to high(a) do
    begin
      seqSum := seqSum + a[j];
      if seqSum > maxSum then
      begin
        maxSum   := seqSum;
        seqStart := i;
        seqEnd   := j;
      end;
    end;
  end;

  writeln ('Sequence: ');
  for i := low(a) to high(a) do
    write (a[i]:3);
  writeln;
  writeln ('Subsequence with greatest sum: ');
  for i := low(a) to seqStart - 1 do
    write (' ':3);
  for i := seqStart to seqEnd do
    write (a[i]:3);
  writeln;
  writeln ('Sum:');
  writeln (maxSum);
end.
