program array2d(input, output);

type
 tArray2d(dim1, dim2: integer) = array[1 .. dim1, 1 .. dim2] of real;
 pArray2D = ^tArray2D;

var
 d1, d2: integer;
 data: pArray2D;

begin
 { read values }
 readln(d1, d2);

 { create array }
 new(data, d1, d2);

 { write element }
 data^[1,1] := 3.5;

 { output element }
 writeln(data^[1,1]);

 { get rid of array }
 dispose(data);
end.
