Program Pi_Spigot;

const
  n   = 1000;
  len = 10*n div 3;

var
  i, j, k, q, x, nines, predigit: integer;
  a: array[1..len] of longint;

begin
  for j := 1 to len do
    a[j] := 2;                 {Start with 2s}
  nines := 0;
  predigit := 0;               {First predigit is a 0}
  for j := 1 to n do
  begin
    q := 0;
    for i := len downto 1 do   {Work backwards}
    begin
      x    := 10*a[i] + q*i;
      a[i] := x mod (2*i - 1);
      q    := x div (2*i - 1);
    end;
    a[1] := q mod 10;
    q := q div 10;
    if q = 9 then
      nines := nines + 1
    else
      if q = 10 then
      begin
	write(predigit+1);
	for k := 1 to nines do
	  write(0);            {zeros}
	predigit := 0;
	nines := 0
      end
      else
      begin
	write(predigit);
	predigit := q;
	if nines <> 0 then
	begin
	  for k := 1 to nines do
	    write(9);
	  nines := 0
	end
      end
  end;
  writeln(predigit);
end.
