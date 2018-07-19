procedure gnomesort(var arr : Array of Integer; size : Integer);
var
   i, j, t	: Integer;
begin
   i := 1;
   j := 2;
   while i < size do begin
      if arr[i-1] <= arr[i] then
      begin
	 i := j;
	 j := j + 1
      end
      else begin
	 t := arr[i-1];
	 arr[i-1] := arr[i];
	 arr[i] := t;
	 i := i - 1;
	 if i = 0 then begin
	    i := j;
	    j := j + 1
	 end
      end
   end;
end;
