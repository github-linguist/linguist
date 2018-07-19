LIBRARY std;
USE std.TEXTIO.all;


entity test is
end entity test;


architecture beh of test is
begin
  process
    variable line_in, line_out : line;
    variable a,b : integer;
  begin
    readline(INPUT, line_in);
    read(line_in, a);
    read(line_in, b);

    write(line_out, a+b);
    writeline(OUTPUT, line_out);
    wait; -- needed to stop the execution
  end process;
end architecture beh;
