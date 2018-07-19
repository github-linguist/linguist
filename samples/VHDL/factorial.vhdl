use std.textio.all;

entity rc is
end entity rc;

architecture beh of rc is
function fact(n:integer) return integer is
    variable f: integer := 1;
    variable i: integer;
begin
    for i in 2 to n loop
        f := f*i;
    end loop;
    return f;
end;

begin
process
    variable i: integer;
    variable l: line;
begin
    for i in 0 to 5 loop
        write(l, i);
        write(l, string'(" "));
        write(l, fact(i));
        writeline(output, l);
    end loop;
    wait;
end process;
end;
