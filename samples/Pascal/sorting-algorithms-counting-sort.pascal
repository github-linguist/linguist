program CountingSort;

procedure counting_sort(var arr : Array of Integer; n, min, max : Integer);
var
   count   : Array of Integer;
   i, j, z : Integer;
begin
   SetLength(count, max-min);
   for i := 0 to (max-min) do
      count[i] := 0;
   for i := 0 to (n-1) do
      count[ arr[i] - min ] := count[ arr[i] - min ] + 1;
   z := 0;
   for i := min to max do
      for j := 0 to (count[i - min] - 1) do begin
	 arr[z] := i;
	 z := z + 1
      end
end;

var
   ages	: Array[0..99] of Integer;
   i	: Integer;

begin
   { testing }
   for i := 0 to 99 do
      ages[i] := 139 - i;
   counting_sort(ages, 100, 0, 140);
   for i := 0 to 99 do
      writeln(ages[i]);
end.
