program large;

  const
     max = 100000000;

  type
     tlist = array[1..max] of longint;

  var
     data : tlist;
	i : integer;

begin

  i := 0;
  while(i < max) 
  do
	begin
		data[i] := 0;
		Writeln(data[i])
	end
end.
