LIBRARY ieee; USE std.TEXTIO.all;
entity quine is end entity quine;
architecture beh of quine is
  type str_array is array(1 to 20) of string(1 to 80);
  constant src : str_array := (
    "LIBRARY ieee; USE std.TEXTIO.all;                                               ",
    "entity quine is end entity quine;                                               ",
    "architecture beh of quine is                                                    ",
    "  type str_array is array(1 to 20) of string(1 to 80);                          ",
    "  constant src : str_array := (                                                 ",
    "begin                                                                           ",
    "  process variable l : line; begin                                              ",
    "    for i in 1 to 5 loop write(l, src(i)); writeline(OUTPUT, l); end loop;      ",
    "    for i in 1 to 20 loop                                                       ",
    "      write(l, character'val(32)&character'val(32));                            ",
    "      write(l, character'val(32)&character'val(32));                            ",
    "      write(l, character'val(34)); write(l, src(i)); write(l,character'val(34));",
    "      if i /= 20 then write(l, character'val(44));                              ",
    "      else            write(l, character'val(41)&character'val(59)); end if;    ",
    "      writeline(OUTPUT, l);                                                     ",
    "    end loop;                                                                   ",
    "    for i in 6 to 20 loop write(l, src(i)); writeline(OUTPUT, l); end loop;     ",
    "    wait;                                                                       ",
    "  end process;                                                                  ",
    "end architecture beh;                                                           ");
begin
  process variable l : line; begin
    for i in 1 to 5 loop write(l, src(i)); writeline(OUTPUT, l); end loop;
    for i in 1 to 20 loop
      write(l, character'val(32)&character'val(32));
      write(l, character'val(32)&character'val(32));
      write(l, character'val(34)); write(l, src(i)); write(l,character'val(34));
      if i /= 20 then write(l, character'val(44));
      else            write(l, character'val(41)&character'val(59)); end if;
      writeline(OUTPUT, l);
    end loop;
    for i in 6 to 20 loop write(l, src(i)); writeline(OUTPUT, l); end loop;
    wait;
  end process;
end architecture beh;
