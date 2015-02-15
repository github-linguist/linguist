with Ada.Text_IO, Ada.Float_Text_IO;

procedure FindMedian is

    f: array(1..10) of float := ( 4.4, 2.3, -1.7, 7.5, 6.6, 0.0, 1.9, 8.2, 9.3, 4.5 );
    min_idx: integer;
    min_val, median_val, swap: float;

begin
    for i in f'range loop
        min_idx := i;
        min_val := f(i);
        for j in i+1 .. f'last loop
            if f(j) < min_val then
                min_idx := j;
                min_val := f(j);
            end if;
        end loop;
        swap := f(i); f(i) := f(min_idx); f(min_idx) := swap;
    end loop;

    if f'length mod 2 /= 0 then
        median_val := f( f'length/2+1 );
    else
        median_val := ( f(f'length/2) + f(f'length/2+1) ) / 2.0;
    end if;

    Ada.Text_IO.Put( "Median value: " );
    Ada.Float_Text_IO.Put( median_val );
    Ada.Text_IO.New_line;
end FindMedian;
