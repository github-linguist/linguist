LIBRARY std;
USE std.TEXTIO.all;

entity test is
end entity test;

architecture beh of test is
begin
  process
    variable line_out : line;
  begin
    write(line_out, string'("Goodbye, World!"));
    writeline(OUTPUT, line_out);
    wait; -- needed to stop the execution
  end process;
end architecture beh;
